/*
    EmacsObserver.java

    @author: <a href="mailto:ovidiu@cup.hp.com">Ovidiu Predescu</a>
    Date: March 29, 2001

 */

package xslt.debugger.cmdline;

import xslt.debugger.Observer;

public class EmacsObserver implements Observer
{
  Controller controller;
  
  public EmacsObserver(Controller controller)
  {
    this.controller = controller;
  }

  public void debuggerProcessStarted()
  {
    System.out.println("<<(xslt-process-debugger-process-started)>>");
  }

  /**
   * Invoked by the debugger to print a descriptive message showing
   * where the debugger stopped.
   *
   * @param filename a <code>String</code> value
   * @param line an <code>int</code> value
   * @param column an <code>int</code> value
   * @param message a <code>String</code> value, additional
   * informative message
   */
  public void debuggerStopped(String filename,
                              int line,
                              int column,
                              String message)
  {
    if (filename.startsWith("file:"))
      filename = filename.substring(5);
    System.out.println("<<(xslt-process-debugger-stopped-at "
                       + "\"" + filename
                       + "\" " + line
                       + " " + column
                       + " \"" + message + "\")>>");
  }

  public void breakpointSetAt(String filename, int line) {}
  
  public void breakpointDeletedAt(String filename, int line) {}
  
  public void breakpointEnabledAt(String filename, int line) {}
  
  public void breakpointDisabledAt(String filename, int line) {}

  /**
   *
   */
  public void stackChanged()
  {
    // TODO: implement this xslt.debugger.Observer method
  }

  /**
   *
   */
  public void frameChanged()
  {
    // TODO: implement this xslt.debugger.Observer method
  }

  public void processorFinished()
  {
    System.out.println("<<(xslt-process-processor-finished)>>");
  }

  public void caughtException(Exception e)
  {
    System.out.println("<<(xslt-process-report-error \""
                       + e.getMessage()
                       + "\")>>");
    
  }
}
