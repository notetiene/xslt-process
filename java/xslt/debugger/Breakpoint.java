/*
    Breakpoint.java

    Author: Ovidiu Predescu <ovidiu@cup.hp.com>
    Date: March  6, 2001

 */

package xslt.debugger;

public class Breakpoint
{
  String filename;
  int line;
  Manager manager;
  boolean enabled = true;

  public Breakpoint (String filename, int line, Manager manager)
  {
    this.filename = filename;
    this.line = line;
    this.manager = manager;
  }

  public void setEnabled(boolean flag)
  {
    enabled = flag;
  }

  public boolean isEnabled()
  {
    return enabled;
  }

  public int hashCode()
  {
    return line;
  }

  public boolean equals(Object obj)
  {
    if (!(obj instanceof Breakpoint))
      return false;

    Breakpoint other = (Breakpoint)obj;
    return line == other.line
      && filename.equals(other.filename)
      && manager == other.manager;
  }

  /**
   * Get the value of filename.
   * @return value of filename.
   */
  public String getFilename()
  {
    return filename;
  }
  
  /**
   * Get the value of line.
   * @return value of line.
   */
  public int getLine()
  {
    return line;
  }
}
