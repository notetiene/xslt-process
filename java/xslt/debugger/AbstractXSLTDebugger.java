/*
    AbstractXSLTDebugger.java

    @author: <a href="mailto:ovidiu@cup.hp.com">Ovidiu Predescu</a>
    Date: March 6, 2001

    Copyright (C) 2001 Ovidiu Predescu

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License as
    published by the Free Software Foundation; either version 2 of the
    License, or (at your option) any later version.
   
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    General Public License for more details.
   
    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
    02111-1307, USA.
 */

package xslt.debugger;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.lang.Runnable;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;
import java.util.ArrayList;
import java.util.HashMap;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParserFactory;
import javax.xml.transform.ErrorListener;
import javax.xml.transform.ErrorListener;
import javax.xml.transform.Source;
import javax.xml.transform.Templates;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.sax.SAXSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;
import org.xml.sax.ErrorHandler;
import org.xml.sax.ErrorHandler;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;
import org.xml.sax.XMLReader;
import xslt.debugger.Manager;
import javax.xml.transform.SourceLocator;

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
  abstract public void prepareTransformerForDebugging(Transformer transformer,
                                                      boolean forDebug);

  public synchronized void run()
  {
    state = RUNNING;
    notifyAll();

    try {
      TransformerFactory tFactory = getTransformerFactory(manager.forDebug);
      tFactory.setErrorListener(new TrAXErrorListener());

      //      File inFile = new File(xmlFilename);
      //      StreamSource in = new StreamSource(xmlFilename);
      System.out.println("run: xmlFilename = " + xmlFilename);
      
      //FileInputStream fis = new FileInputStream(xmlFilename);
      InputSource is = new InputSource(new URL(xmlFilename).toString());
      SAXSource saxSource = new SAXSource(is);
      setupXMLReader(saxSource);
      
      String media = null, title = null, charset = null;
      Source stylesheetSource
        = tFactory.getAssociatedStylesheet(saxSource, media, title, charset);
      // Set an error handler for the stylesheet parser
      if (stylesheetSource instanceof SAXSource)
        setupXMLReader((SAXSource)stylesheetSource);

      StreamResult result = new StreamResult(manager.getOutStream());
      String stylesheetId = stylesheetSource.getSystemId();

      // Check for a Templates object already created for this
      // stylesheet
      XSLTSheetInfo sheetInfo = (XSLTSheetInfo)sheetCache.get(stylesheetId);
      Templates template;
      
      if (sheetInfo == null) {
        sheetInfo = new XSLTSheetInfo(stylesheetSource);
        sheetCache.put(stylesheetId, sheetInfo);
        template = sheetInfo.template;
      }
      else {
        template = sheetInfo.getTemplates(stylesheetSource);
      }

      Transformer transformer = template.newTransformer();
      prepareTransformerForDebugging(transformer, manager.forDebug);

      if (transformer != null)
        transformer.transform(saxSource, result);
    }
    catch(Exception e) {
      manager.getObserver().caughtException(e);
    }
    finally {
      manager.getObserver().processorFinished();
      state = NOT_RUNNING;
      action = DO_NOTHING;
      notifyAll();
    }
  }

  /**
   * <code>setupXMLReader</code> sets up an ErrorHandler for a given
   * SAXSource object.
   *
   * @param saxSource the <code>SAXSource</code> object
   */
  public void setupXMLReader(SAXSource saxSource)
    throws SAXException, ParserConfigurationException
  {
    XMLReader xmlReader = saxSource.getXMLReader();
    if (xmlReader == null) {
      xmlReader = SAXParserFactory.newInstance()
        .newSAXParser().getXMLReader();
      saxSource.setXMLReader(xmlReader);
    }
    xmlReader.setErrorHandler(new SaxParserErrorHandler());
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

    public XSLTSheetInfo(Source stylesheetSource)
      throws TransformerConfigurationException
    {
      System.out.println("new XSLTSheetInfo: " + stylesheetSource.getSystemId());
      
      TransformerFactory tFactory = getTransformerFactory(manager.forDebug);
      template = tFactory.newTemplates(stylesheetSource);
      lastModified = getLastModified(stylesheetSource.getSystemId());
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

    public Templates getTemplates(Source stylesheetSource)
      throws TransformerConfigurationException
    {
      String stylesheetId = stylesheetSource.getSystemId();
      long currentTime = Long.MIN_VALUE;

      currentTime = getLastModified(stylesheetId);

      if (template != null && currentTime <= lastModified) {
        // The XSLT sheet has not been modified since the last access,
        // return the cached one
        return template;
      }

      // The XSLT sheet has been modified, create a new template
      // and return it
      TransformerFactory tFactory = getTransformerFactory(manager.forDebug);
      template = tFactory.newTemplates(stylesheetSource);
      lastModified = currentTime;

      return template;
    }
  }

  /**
   * Implementation of the
   * <code>java.xml.transform.ErrorListener</code> interface in the
   * TrAX interface, and is used to report error to an
   * <code>Observer</code> instance.
   *
   * @author <a href="mailto:ovidiu@cup.hp.com">Ovidiu Predescu</a>
   */
  class TrAXErrorListener implements ErrorListener
  {
    public TrAXErrorListener() {}
    
    public void error(TransformerException ex)
      throws TransformerException
    {
      reportError("error", ex);
    }

    public void warning(TransformerException ex)
      throws TransformerException
    {
      reportError("warning", ex);
    }

    public void fatalError(TransformerException ex)
      throws TransformerException
    {
      reportError("fatal error", ex);
    }

    public void reportError(String type, TransformerException ex)
      throws TransformerException
    {
      //      System.out.println("TrAXErrorListener " + type + ": " + ex);
      SourceLocator locator = ex.getLocator();
      String message = locator.getSystemId() + "\n"
        + locator.getLineNumber() + ":" + locator.getColumnNumber();
      TransformerException newEx = new TransformerException(message);
      newEx.initCause(ex);
      //      manager.getObserver().caughtException(newEx);
      throw newEx;
    }
  }

  /**
   * <code>SaxParserErrorHandler</code> is an implementation of the
   * <code>org.xml.sax.ErrorHandler</code> interface, and is used to
   * report error to an <code>Observer</code> instance.
   *
   * @author <a href="mailto:ovidiu@cup.hp.com">Ovidiu Predescu</a>
   */
  class SaxParserErrorHandler implements ErrorHandler
  {
    public void error(SAXParseException ex)
      throws SAXException
    {
      reportError("error", ex);
    }

    public void warning(SAXParseException ex)
      throws SAXException
    {
      reportError("warning", ex);
    }

    public void fatalError(SAXParseException ex)
      throws SAXException
    {
      reportError("fatal error", ex);
    }

    public void reportError(String type, SAXParseException ex)
      throws SAXException
    {
      //      System.out.println("SaxParserErrorHandler " + type + ": " + ex);
      String message = ex.getSystemId() + "\n"
        + ex.getLineNumber() + ":" + ex.getColumnNumber();
      SAXException newEx = new SAXException(message, ex);
      //      manager.getObserver().caughtException(newEx);
      throw newEx;
    }
  }
}
