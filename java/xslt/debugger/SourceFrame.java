/*
    SourceFrame.java

    Author: Ovidiu Predescu <ovidiu@cup.hp.com>
    Date: March  6, 2001

 */

package xslt.debugger;

import java.util.ArrayList;
import java.lang.Cloneable;

public class SourceFrame implements Cloneable
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

  protected Object clone()
  {
    SourceFrame frame = null;

    try {
      Class thisClass = this.getClass();
      frame = (SourceFrame)thisClass.newInstance();
      frame.name = name;
      frame.filename = filename;
      frame.line = line;
      frame.column = column;
      frame.manager = manager;
      frame.styleFrame = styleFrame;
    }
    catch (Exception e) {
      System.out.println("Cannot clone object " + this + ": " + e.toString());
    }

    return frame;
  }

  public boolean equals(Object object)
  {
    if (object instanceof SourceFrame) {
      SourceFrame frame = (SourceFrame)object;
      return name.equals(frame.name)
        && line == frame.line
        && column == frame.column
        && (styleFrame == frame.styleFrame
            || styleFrame.equals(frame.styleFrame));
    }
    else
      return false;
  }

  public int hashCode()
  {
    return filename.hashCode() + line;
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

  /**
   * Get the file name of this source frame.
   *
   * @return an <code>int</code> value
   */
  public String getFilename()
  {
    return filename;
  }

  /**
   * Get the line number of this source frame.
   *
   * @return an <code>int</code> value
   */
  public int getLine()
  {
    return line;
  }
}

