/*
    XalanTraceListener.java

    @author: <a href="mailto:ovidiu@cup.hp.com">Ovidiu Predescu</a>
    Date: April 26, 2001

 */

package xslt.debugger.xalan;


import java.io.PrintWriter;
import org.apache.xalan.templates.ElemTemplateElement;
import org.apache.xalan.trace.GenerateEvent;
import org.apache.xalan.trace.SelectionEvent;
import org.apache.xalan.trace.TraceListener;
import org.apache.xalan.trace.TracerEvent;
import org.w3c.dom.Node;
import org.apache.xpath.XPath;
import javax.xml.transform.SourceLocator;

public class XalanTraceListener implements TraceListener
{
  public XalanTraceListener() {}

  public void generated(GenerateEvent ev)
  {
  }

  public void selected(SelectionEvent ev)
  {
    // TODO
    Node sourceNode = ev.m_sourceNode;
    XPath xpath = ev.m_xpath;
    SourceLocator locator = xpath.getLocator();
    System.out.println("source: line " + locator.getLineNumber()
                       + ", file " + locator.getSystemId()
                       + ", element " + sourceNode.getNodeName());
    
  }

  public void trace(TracerEvent ev)
  {
    // TODO
    Node sourceNode = ev.m_sourceNode;
    ElemTemplateElement styleNode = ev.m_styleNode;

    System.out.println("style: line " + styleNode.getLineNumber()
                       + ", name " + styleNode.getSystemId());
    
  }
}
