/*
    Variable.java

    Author: Ovidiu Predescu <ovidiu@cup.hp.com>
    Date: March  6, 2001

 */

package xslt.debugger;

public class Variable
{
  protected String name;
  protected Value value;
  protected StyleFrame frame;

  public Variable (String name, StyleFrame frame)
  {
    this.name = name;
    this.frame = frame;
  }

  /**
   * Set the value of this variable
   * @param value a <code>Value</code> value
   */
  public void setValue(Value value)
  {
    this.value = value;
  }

  /**
   * Get the value of the variable
   * @return an <code>Object</code> value
   */
  public Value getValue()
  {
    return value;
  }

  /**
   * Get the name of the variable.
   * @return value of name.
   */
  public String getName()
  {
    return name;
  }

  /**
   * Get the style frame this variable belongs to
   * @return a <code>StyleFrame</code> value
   */
  public StyleFrame getFrame()
  {
    return frame;
  }
}
