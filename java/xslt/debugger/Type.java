/*
    Type.java

    @author: <a href="mailto:ovidiu@cup.hp.com">Ovidiu Predescu</a>
    Date: March 22, 2001

 */

package xslt.debugger;

public class Type 
{
  public static final String BOOLEAN = "boolean";
  public static final String NUMBER = "number";
  public static final String STRING = "string";
  public static final String NODESET = "nodeset";
  public static final String OBJECT = "object";
  public static final String ANY = "any";

  protected String type;

  public Type(String type)
  {
    this.type = type;
  }

  public String getTypeName()
  {
    return type;
  }
}
