/*
    Observer.java

    Author: Ovidiu Predescu <ovidiu@cup.hp.com>
    Date: March  6, 2001

 */

package xslt.debugger;

public interface Observer
{
  /**
   * Called immediately after the debugger process is created. Use
   * this method to setup any global variables you might need for the
   * lifetime of the debugger process. This method is called only once
   * during the life of the debugger process.
   */
  public void debuggerProcessStarted();

  /**
   * Called to notify the debugger is stopped, at
   * <code>filename</code> in line <code>line</code> and column
   * <code>column</code>.
   *
   * The <code>message</code> contains a string starting either with
   * "entering: " or with "leaving: ", depending on whether the XSLT
   * processor in entering or leaving a particular element. Following
   * this, the display name of the element being visited is appended.
   *
   * @param filename a <code>String</code> value
   * @param lineno an <code>int</code> value
   * @param column an <code>int</code> value
   * @param message a <code>String</code> value
   */
  public void debuggerStopped(String filename,
                              int lineno,
                              int column,
                              String message);

  /**
   * Called to inform that a breakpoint was set in
   * <code>filename</code> at <code>line</code>.
   *
   * @param filename a <code>String</code> value
   * @param line an <code>int</code> value
   */
  public void breakpointSetAt(String filename, int line);

  /**
   * Called to inform that a breakpoint was deleted in
   * <code>filename</code> at <code>line</code>.
   *
   * @param filename a <code>String</code> value
   * @param line an <code>int</code> value
   */
  public void breakpointDeletedAt(String filename, int line);

  /**
   * Called to inform that the breakpoint in <code>filename</code> at
   * <code>line</code> was enabled.
   *
   * @param filename a <code>String</code> value
   * @param line an <code>int</code> value
   */
  public void breakpointEnabledAt(String filename, int line);

  /**
   * Called to inform that the breakpoint in <code>filename</code> at
   * <code>line</code> was disabled.
   *
   * @param filename a <code>String</code> value
   * @param line an <code>int</code> value
   */
  public void breakpointDisabledAt(String filename, int line);

  /**
   * Called to notify that the source frames stack has changed. This
   * method is called only if the source frames stack has changed when
   * the debugger stops. It is not called during the normal execution
   * of the XSLT processor.
   */
  public void sourceStackChanged();

  /**
   * Called to notify that the style frames stack has changed. This
   * method is called only if the style frames stack has changed when
   * the debugger stops. It is not called during the normal execution
   * of the XSLT processor.
   */
  public void styleStackChanged();

  /**
   * Notifies that the XSLT processing has just finished.
   */
  public void processorFinished();

  /**
   * Notifies that an exception occurred while running the XSLT
   * debugger process.
   *
   * @param e an <code>Exception</code> value
   */
  public void caughtException(Exception e);
}
