/*
    CmdLineObserver.java

    @author: <a href="mailto:ovidiu@cup.hp.com">Ovidiu Predescu</a>
    Date: March 29, 2001

 */

package xslt.debugger.cmdline;

import xslt.debugger.Observer;

public class CmdLineObserver implements Observer
{
  Controller controller;
  
  public CmdLineObserver(Controller controller)
  {
    this.controller = controller;
  }

  public void debuggerProcessStarted()
  {
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
    System.out.println("Debugger stopped at: " + filename
                       + " " + line
                       + ":" + column
                       + " " + message);
  }

  public void breakpointSetAt(String filename, int line)
  {
    System.out.println("Set breakpoint at " + filename + " " + line);
  }
  
  public void breakpointDeletedAt(String filename, int line)
  {
    System.out.println("Deleted breakpoint at " + filename + " " + line);
  }
  
  public void breakpointEnabledAt(String filename, int line)
  {
    System.out.println("Enabled breakpoint at " + filename + " " + line);
  }
  
  public void breakpointDisabledAt(String filename, int line)
  {
    System.out.println("Disabled breakpoint at " + filename + " " + line);
  }

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
  }

  public void caughtException(Exception e)
  {
    e.printStackTrace();
  }
}
