/*
    StyleFrame.java

    Author: Ovidiu Predescu <ovidiu@cup.hp.com>
    Date: March  6, 2001

 */

package xslt.debugger;

import java.util.ArrayList;
import java.lang.Cloneable;
import java.lang.CloneNotSupportedException;

public class StyleFrame implements Cloneable
{
  protected ArrayList localVariables = null;
  String name;
  String filename;
  int line;
  int column;
  boolean isExiting = false;
  Manager manager;
  SourceFrame sourceFrame;
  
  public StyleFrame (String name,
                     String filename,
                     int line,
                     int column,
                     Manager manager)
  {
    this.name = name;
    if (filename.startsWith("file:"))
      filename = filename.substring(5);
    this.filename = filename;
    this.line = line;
    this.column = column;
    this.manager = manager;
  }

  protected Object clone()
    throws CloneNotSupportedException
  {
    StyleFrame frame = null;

    try {
      Class thisClass = this.getClass();
      frame = (StyleFrame)thisClass.newInstance();
      frame.localVariables = (localVariables == null
                              ? null
                              :(ArrayList)localVariables.clone());
      frame.name = name;
      frame.filename = filename;
      frame.line = line;
      frame.column = column;
      frame.manager = manager;
      frame.sourceFrame = sourceFrame;
    }
    catch (Exception e) {
      System.out.println("Cannot clone object " + this + ": " + e.toString());
    }

    return frame;
  }

  public boolean equals(Object object)
  {
    if (object instanceof StyleFrame) {
      StyleFrame frame = (StyleFrame)object;
      return filename.equals(frame.filename)
        && line == frame.line
        && column == frame.column
        && (sourceFrame == frame.sourceFrame
            || sourceFrame.equals(frame.sourceFrame))
        && (localVariables == frame.localVariables
            || localVariables.equals(frame.localVariables));
    }
    else
      return false;
  }

  public int hashCode()
  {
    return filename.hashCode() + line;
  }

  /**
   * Return all the local variables in this style frame. Always use
   * this method to get the local variables, as specific subclasses
   * may delay the creation of the variables list until invoked.
   *
   * @return an <code>ArrayList</code> value whose elements are
   * Variable objects
   */
  public ArrayList getLocalVariables()
  {
    return localVariables;
  }

  /**
   * Return the value of a local variable.
   *
   * @param name a <code>String</code> value
   * @return a <code>Value</code> value
   */
  public Value getValueOfLocalVariable(String name)
    throws NoSuchFieldException
  {
    // Find the Variable object corresponding to this variable
    ArrayList variables = getLocalVariables();

    if (variables == null)
      throw new NoSuchFieldException();
    
    Variable variable = null;
    
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
   * Get the value of sourceFrame.
   * @return value of sourceFrame.
   */
  public SourceFrame getSourceFrame()
  {
    return sourceFrame;
  }
  
  /**
   * Set the value of sourceFrame.
   * @param sourceFrame Value to assign to sourceFrame.
   */
  public void setSourceFrame(SourceFrame sourceFrame)
  {
    this.sourceFrame = sourceFrame;
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
   * Get the file name of this style frame.
   *
   * @return an <code>int</code> value
   */
  public String getFilename()
  {
    return filename;
  }

  /**
   * Get the line number of this style frame.
   *
   * @return an <code>int</code> value
   */
  public int getLine()
  {
    return line;
  }

  /**
   * Sets the <code>isExiting</code> indicator to show the debugger is
   * currently exiting from this element.
   */
  public void setIsExiting()
  {
    isExiting = true;
  }

  /**
   * Returns the <code>isExiting</code> indicator.
   *
   * @return a <code>boolean</code> value
   */
  public boolean isExiting()
  {
    return isExiting;
  }
}
