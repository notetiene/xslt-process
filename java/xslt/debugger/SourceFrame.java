/*
    SourceFrame.java

    Author: Ovidiu Predescu <ovidiu@cup.hp.com>
    Date: March  6, 2001

 */

package xslt.debugger;

import java.util.ArrayList;

public class SourceFrame
{
  String name;
  String filename;
  int line;
  int column;
  Manager manager;
  StyleFrame styleFrame = null;

  /**
   * Creates a new <code>SourceFrame</code> instance.
   *
   * @param name a <code>String</code> value
   * @param filename a <code>String</code> value
   * @param line an <code>int</code> value
   * @param column an <code>int</code> value
   * @param manager a <code>Manager</code> value
   */
  public SourceFrame (String name,
                      String filename,
                      int line,
                      int column,
                      Manager manager)
  {
    this.name = name;
    this.filename = filename;
    this.line = line;
    this.column = column;
    this.manager = manager;
  }
  
  /**
   * Get the value of frame.
   * @return value of frame.
   */
  public StyleFrame getStyleFrame()
  {
    return styleFrame;
  }
  
  /**
   * Set the value of frame.
   * @param styleFrame a <code>StyleFrame</code> value
   */
  public void setStyleFrame(StyleFrame styleFrame)
  {
    this.styleFrame = styleFrame;
  }
  
  /**
   * Get the value of name.
   * @return value of name.
   */
  public String getName()
  {
    return name;
  }
  
  /**
   * Set the value of name.
   * @param name  Value to assign to name.
   */
  public void setName(String name)
  {
    this.name = name;
  }
}

