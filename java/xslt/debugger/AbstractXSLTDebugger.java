/*
    AbstractXSLTDebugger.java

    @author: <a href="mailto:ovidiu@cup.hp.com">Ovidiu Predescu</a>
    Date: March 6, 2001

 */

package xslt.debugger;


import java.io.OutputStream;
import java.lang.Runnable;
import java.util.ArrayList;
import java.util.HashMap;
import javax.xml.transform.Templates;
import xslt.debugger.Manager;
import java.io.File;
import javax.xml.transform.stream.StreamSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.Source;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.TransformerConfigurationException;
import java.net.URL;
import java.net.URLConnection;
import java.net.MalformedURLException;
import java.io.IOException;
import javax.xml.transform.Transformer;

public abstract class AbstractXSLTDebugger implements Runnable
{
  // These are the possible states in which the worker thread can be
  public static final int NOT_RUNNING = 1; // the worker thread is not
                                           // running
  public static final int RUNNING = 2; // the worker thread is running
  public static final int STOPPED = 3; // the worker thread is stopped
                                       // at a breakpoint or manually
                                       // stopped using 'stop'

  // These are the possible actions that could be taken on a worker
  // thread
  public static final int DO_NOTHING = 0;
  public static final int DO_STOP = 1;
  public static final int DO_STEP = 2;
  public static final int DO_NEXT = 3;
  public static final int DO_FINISH = 4;
  public static final int DO_CONTINUE = 5;
  public static final int DO_DEFERRED_STOP = 6;

  String processorName = null;
  protected HashMap sheetCache = new HashMap();
  
  protected Manager manager = null;
  protected String xmlFilename;

  protected int state = NOT_RUNNING;
  protected int action = DO_NOTHING;

  public AbstractXSLTDebugger() {}

  public synchronized boolean isStarted()
  {
    return state != NOT_RUNNING;
  }

  public synchronized void stopProcessing()
    throws InterruptedException
  {
    action = DO_STOP;
    notifyAll();
    while (state != STOPPED)
      wait();
  }

  abstract public TransformerFactory getTransformerFactory(boolean forDebug);
  abstract public void prepareTransformerForDebugging(Transformer transformer);

  public synchronized void run()
  {
    state = RUNNING;
    notifyAll();

    try {
      TransformerFactory tFactory = getTransformerFactory(manager.forDebug);
      File inFile = new File(xmlFilename);
      StreamSource in = new StreamSource(xmlFilename);
      StreamResult result = new StreamResult(manager.getOutStream());

      String media = null, title = null, charset = null;
      Source stylesheet
        = tFactory.getAssociatedStylesheet(in, media, title, charset);
      String stylesheetId = stylesheet.getSystemId();

      // Check for a Templates object already created for this
      // stylesheet
      XSLTSheetInfo sheetInfo = (XSLTSheetInfo)sheetCache.get(stylesheetId);
      Templates template;
      
      if (sheetInfo == null) {
        sheetInfo = new XSLTSheetInfo(stylesheet);
        sheetCache.put(stylesheetId, sheetInfo);
        template = sheetInfo.template;
      }
      else {
        template = sheetInfo.getTemplates(stylesheet);
      }

      Transformer transformer = template.newTransformer();
      if (manager.forDebug)
        prepareTransformerForDebugging(transformer);

      if (transformer != null)
        transformer.transform(in, result);

      manager.getObserver().processorFinished();
    }
    catch(Exception e) {
      manager.getObserver().caughtException(e);
    }

    state = NOT_RUNNING;
    notifyAll();
  }

  public synchronized void checkRequestToStop()
  {
    if (action == DO_STOP) {
      state = STOPPED;
      notifyAll();
    }
  }
  
  public synchronized void debuggerStopped(String filename,
                                           int line,
                                           int column,
                                           String message)
  {
    try {
      state = STOPPED;
      notifyAll();
      manager.observer.debuggerStopped(filename, line, column, message);
      while (state != RUNNING)
        wait();
    }
    catch (InterruptedException e) {
    }
  }

  public synchronized void doStep()
  {
    state = RUNNING;
    action = DO_STEP;
    notifyAll();
  }

  public synchronized void doNext()
  {
    state = RUNNING;
    action = DO_NEXT;
    notifyAll();
  }

  public synchronized void doFinish()
  {
    state = RUNNING;
    action = DO_FINISH;
    notifyAll();
  }

  public synchronized void doContinue()
  {
    state = RUNNING;
    action = DO_CONTINUE;
    notifyAll();
  }

  public abstract ArrayList getGlobalVariables();

  public Value getValueOfGlobalVariable(String name)
    throws NoSuchFieldException
  {
    ArrayList variables = getGlobalVariables();
    Variable variable = null;

    if (variables == null)
      throw new NoSuchFieldException();

    for (int i = 0; i < variables.size(); i++) {
      Variable var = (Variable)variables.get(i);
      String localName = var.getName();
      if (localName.equals(name)) {
        variable = var;
        break;
      }
    }

    if (variable == null)
      throw new NoSuchFieldException();

    return variable.getValue();
  }

  /**
   * <code>getAction</code> returns the action to be taken next.
   *
   * @return an <code>int</code> value
   */
  public int getAction()
  {
    return action;
  }

  public void setAction(int action)
  {
    this.action = action;
  }

  public void setManager(Manager manager)
  {
    this.manager = manager;
  }

  public Manager getManager() 
  {
    return manager;
  }

  public void setXmlFilename(String filename)
  {
    xmlFilename = filename;
  }

  public void setProcessorName(String name)
  {
    this.processorName = name;
  }

  public String getProcessorName()
  {
    return processorName;
  }

  /**
   * <code>XSLTSheetInfo</code> is used by {@link
   * AbstractXSLTDebugger} to maintain cached templates.
   */
  class XSLTSheetInfo
  {
    Templates template = null;
    long lastModified = Long.MIN_VALUE;

    public XSLTSheetInfo(Source stylesheet)
    {
      try {
        TransformerFactory tFactory = getTransformerFactory(manager.forDebug);
        template = tFactory.newTemplates(stylesheet);
        lastModified = getLastModified(stylesheet.getSystemId());
      }
      catch (TransformerConfigurationException e) {}
    }

    long getLastModified(String stylesheetId)
    {
      long lastModified = Long.MIN_VALUE;
      URL url = null;

      try {
        url = new URL(stylesheetId);
        if (url.getProtocol().equals("file")) {
          File file = new File(url.getFile());
          lastModified = file.lastModified();
        }
        else {
          URLConnection conn = url.openConnection();
          conn.connect();
          lastModified = conn.getLastModified();
        }
      }
      catch (MalformedURLException e) {
        System.err.println("Invalid URL " + url + ": " + e.toString());
      }
      catch (IOException e) {
        System.err.println("Cannot access " + url + ": "+ e.toString());
      }

      return lastModified;
    }

    public Templates getTemplates(Source stylesheet)
    {
      String stylesheetId = stylesheet.getSystemId();
      long currentTime = Long.MIN_VALUE;

      currentTime = getLastModified(stylesheetId);

      if (currentTime <= lastModified && template != null) {
        // The XSLT sheet has not been modified since the last access,
        // return the cached one
        return template;
      }

      // The XSLT sheet has been modified, create a new template
      // and return it
      try {
        TransformerFactory tFactory = getTransformerFactory(manager.forDebug);
        template = tFactory.newTemplates(stylesheet);
        lastModified = currentTime;
      }
      catch (TransformerConfigurationException e) {
        System.out.println("Could not create transformer for: "
                           + stylesheetId);
      }

      return template;
    }
  }
}
