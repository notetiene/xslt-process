/*
    SaxonType.java

    @author: <a href="mailto:ovidiu@cup.hp.com">Ovidiu Predescu</a>
    Date: March 22, 2001

 */

package xslt.debugger.saxon;

import com.icl.saxon.expr.Value;

import xslt.debugger.Type;

public class SaxonType extends Type
{
  public SaxonType(int saxonType)
  {
    super(null);

    switch (saxonType) {
    case Value.BOOLEAN:
      type = Type.BOOLEAN;
      break;
    case Value.NUMBER:
      type = Type.BOOLEAN;
      break;
    case Value.STRING:
      type = Type.STRING;
      break;
    case Value.NODESET:
      type = Type.NODESET;
      break;
    case Value.OBJECT:
      type = Type.OBJECT;
      break;
    default:
      type = Type.ANY;
      break;
    }
  }
}
