/*
    AbstractXSLTDebugger.java

    @author: <a href="mailto:ovidiu@cup.hp.com">Ovidiu Predescu</a>
    Date: March 6, 2001

 */

package xslt.debugger;


import java.util.ArrayList;
import java.lang.Runnable;
import java.io.OutputStream;

import xslt.debugger.Manager;

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
  protected Manager manager = null;
  protected String xmlFilename;

  // The default output stream is stdout
  protected OutputStream outStream = System.out;

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

  abstract public void run();

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

  public void setOutStream(OutputStream stream)
  {
    outStream = stream;
  }

  public void setProcessorName(String name)
  {
    this.processorName = name;
  }

  public String getProcessorName()
  {
    return processorName;
  }
}
