/*
    SaxonVariable.java

    @author: <a href="mailto:ovidiu@cup.hp.com">Ovidiu Predescu</a>
    Date: March 20, 2001

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

public class SaxonVariable extends Variable
{
  public SaxonVariable(String name,
                       Context context,
                       StyleElement element,
                       int frameId,
                       StyleFrame styleFrame)
  {
    super(name, styleFrame);
    this.value = new SaxonValue(this, context, element, frameId);
  }
}
