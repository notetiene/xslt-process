/*
    Observer.java

    Author: Ovidiu Predescu <ovidiu@cup.hp.com>
    Date: March  6, 2001

 */

package xslt.debugger;

public interface Observer
{
  public void debuggerStopped(String filename,
                              int lineno,
                              int column,
                              String message);
  public void breakpointSetAt(String filename, int line);
  public void breakpointDeletedAt(String filename, int line);
  public void breakpointEnabledAt(String filename, int line);
  public void breakpointDisabledAt(String filename, int line);
  public void stackChanged();
  public void frameChanged();
  public void processorFinished();
  public void caughtException(Exception e);
}
