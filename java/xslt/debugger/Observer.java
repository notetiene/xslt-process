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
  public void stackChanged();
  public void frameChanged();
}
