/*
    Value.java

    @author: <a href="mailto:ovidiu@cup.hp.com">Ovidiu Predescu</a>
    Date: March 22, 2001

 */

package xslt.debugger;

public class Value 
{
  protected Type type;
  protected String value;

  public Value() {}

  /**
   * Get the type of this value object
   * @return value of type.
   */
  public Type getType()
  {
    return type;
  }
  
  /**
   * Set the value of type.
   * @param type  Value to assign to type.
   */
  public void setType(Type type)
  {
    this.type = type;
  }

  /**
   * Get the value hold by this instance.
   * @return value of value.
   */
  public String getValue()
  {
    return value;
  }
  
  /**
   * Set the value of this instance.
   * @param value  Value to assign to value.
   */
  public void setValue(String value)
  {
    this.value = value;
  }
}
