/*
    SaxonValue.java

    @author: <a href="mailto:ovidiu@cup.hp.com">Ovidiu Predescu</a>
    Date: March 22, 2001

 */

package xslt.debugger.saxon;

import com.icl.saxon.Binding;
import com.icl.saxon.Context;
import com.icl.saxon.expr.Value;
import com.icl.saxon.expr.XPathException;
import com.icl.saxon.om.NamePool;
import com.icl.saxon.style.StyleElement;

import xslt.debugger.Manager;
import xslt.debugger.StyleFrame;
import xslt.debugger.Variable;
import xslt.debugger.Type;

public class SaxonValue extends xslt.debugger.Value
{
  SaxonVariable variable;
  Context context;
  StyleElement element;
  int frameId;

  public SaxonValue(SaxonVariable variable,
                    Context context,
                    StyleElement element,
                    int frameId)
  {
    super();
    this.variable = variable;
    this.context = context;
    this.element = element;
    this.frameId = frameId;
  }

  public String getValue()
  {
    try {
      if (value == null) {
        System.out.println("element is " + element.getClass());
        
        NamePool namePool = element.getNamePool();
        System.out.println("namePool = " + namePool);
        
        int fprint = namePool.getCodeForURI(variable.getName());
        System.out.println("fingerprint for name " + variable.getName()
                           + " is " + fprint);
        
        Binding b = element.getVariableBinding(fprint);
        System.out.println("binding is " + b);
        
        if (b != null) {
          Value v = context.getBindery().getValue(b, frameId);
          System.out.println("saxon value: " + v);
          
          if (v != null) {
            value = v.asString();
            type = new SaxonType(v.getDataType());
          }
        }
      }
    }
    catch (XPathException e) {
    }

    return value;
  }
}
