/*
    StyleFrame.java

    Author: Ovidiu Predescu <ovidiu@cup.hp.com>
    Date: March  6, 2001

 */

package xslt.debugger;

import java.util.ArrayList;

public class StyleFrame
{
  ArrayList variables = null;
  String name;
  String filename;
  int line;
  int column;
  Manager manager;
  SourceFrame sourceFrame;
  
  public StyleFrame (String name,
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
   * Return all the local variables in this style frame. Always use
   * this method to get the local variables, as specific subclasses
   * may delay the creation of the variables list until invoked.
   *
   * @return an <code>ArrayList</code> value whose elements are
   * Variable objects
   */
  public ArrayList getLocalVariables()
  {
    return variables;
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
  
}
