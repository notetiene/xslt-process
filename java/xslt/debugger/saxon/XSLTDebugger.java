/*
    XSLTDebugger.java

    @author: <a href="mailto:ovidiu@cup.hp.com">Ovidiu Predescu</a>
    Date: March 6, 2001

 */

package xslt.debugger.saxon;

import com.icl.saxon.Controller;
import com.icl.saxon.ExtendedInputSource;
import com.icl.saxon.FeatureKeys;
import com.icl.saxon.ParameterSet;
import com.icl.saxon.StyleSheet;
import com.icl.saxon.output.Emitter;
import com.icl.saxon.style.StyleElement;
import com.icl.saxon.trace.TraceListener;
import java.io.File;
import java.io.FileOutputStream;
import java.lang.Runnable;
import java.util.ArrayList;
import java.util.Stack;
import javax.xml.transform.Source;
import javax.xml.transform.Templates;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;
import xslt.debugger.AbstractXSLTDebugger;
import xslt.debugger.Manager;
import xslt.debugger.Observer;
import xslt.debugger.cmdline.EmacsObserver;
import com.icl.saxon.output.MessageEmitter;
import java.io.PrintWriter;

public class XSLTDebugger extends AbstractXSLTDebugger
{
  TransformerFactory tFactory = null;
  TransformerFactory tDebugFactory = null;
  
  public XSLTDebugger() {}

  public TransformerFactory getTransformerFactory(boolean forDebug)
  {
    if (tFactory == null) {
      tFactory = new com.icl.saxon.TransformerFactoryImpl();
      tDebugFactory = new com.icl.saxon.TransformerFactoryImpl();

      TraceListener traceListener = new SaxonTraceListener(this);
      tDebugFactory.setAttribute(FeatureKeys.TRACE_LISTENER, traceListener);
      tDebugFactory.setAttribute(FeatureKeys.LINE_NUMBERING, Boolean.TRUE);
    }

    return forDebug ? tDebugFactory : tFactory;
  }
  
  /**
   * Prepares the transformer for debugging.
   *
   * If the observer is an EmacsObserver, changes the Saxon
   * <code>Emitter</code>, used to output <code>xsl:message</code>, to
   * <code>EmacsEmitter</code>.
   */
  public void prepareTransformerForDebugging(Transformer transformer)
  {
    Observer observer = manager.getObserver();
    if (observer instanceof EmacsObserver) {
      MessageEmitter emitter = new MessageEmitter();
      emitter.setWriter(new PrintWriter(manager.getMessageStream()));
      emitter.setOutputStream(manager.getMessageStream());
      ((Controller)transformer).setMessageEmitter(emitter);
    }
  }

  /**
   * Return the list of global variables.
   *
   * @return an <code>ArrayList</code> value
   */
  public ArrayList getGlobalVariables()
  {
    // With Saxon we obtain the list of global variables by asking a
    // SaxonStyleFrame object for it. We need to find a StyleElement
    // in the style frames stack to be able to ask for global
    // variables.
    Stack styleFrames = manager.getStyleFrames();
    SaxonStyleFrame styleFrame = (SaxonStyleFrame)styleFrames.get(0);
    if (styleFrame.getElement() instanceof StyleElement)
      return styleFrame.getGlobalVariables();
    else
      return null;
  }
}
