/*
    XSLTDebugger.java

    Author: Ovidiu Predescu <ovidiu@cup.hp.com>
    Date: April 10, 2001

 */

package xslt.debugger.xalan;

import javax.xml.transform.stream.StreamSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.Transformer;
import javax.xml.transform.Templates;
import javax.xml.transform.Source;
import java.util.TooManyListenersException;
import java.util.Stack;
import java.util.ArrayList;
import java.lang.Runnable;
import java.io.PrintWriter;
import java.io.FileOutputStream;
import java.io.File;

import xslt.debugger.Manager;
import xslt.debugger.AbstractXSLTDebugger;
import org.apache.xalan.transformer.TransformerImpl;
import org.apache.xalan.trace.TraceManager;
import org.apache.xalan.trace.TraceListener;
import org.apache.xalan.trace.PrintTraceListener;

public class XSLTDebugger extends AbstractXSLTDebugger
{
  TransformerFactory tFactory = null;

  public XSLTDebugger() {}

  public TransformerFactory getTransformerFactory(boolean forDebug)
  {
    if (tFactory == null)
      tFactory = new org.apache.xalan.processor.TransformerFactoryImpl();

    return tFactory;
  }

  public void prepareTransformerForDebugging(Transformer transformer,
                                             boolean forDebug)
  {
    if (forDebug) {
      PrintWriter diagnosticsWriter = new PrintWriter(System.err, true);
      PrintTraceListener traceListener
        = new PrintTraceListener(diagnosticsWriter);
      traceListener.m_traceTemplates = true;
      traceListener.m_traceElements = true;
      traceListener.m_traceGeneration = true;
      traceListener.m_traceSelection = true;

      TransformerImpl transformerImpl = (TransformerImpl)transformer;
      // Register the TraceListener with a TraceManager associated
      // with the TransformerImpl.
      TraceManager trMgr = transformerImpl.getTraceManager();
      try {
        trMgr.addTraceListener(traceListener);
      }
      catch (TooManyListenersException e) {
        manager.getObserver().caughtException(e);
      }
    }
  }

  public ArrayList getGlobalVariables()
  {
    return null;
  }
//   /**
//    * Return the list of global variables.
//    *
//    * @return an <code>ArrayList</code> value
//    */
//   public ArrayList getGlobalVariables()
//   {
//     // With Saxon we obtain the list of global variables by asking a
//     // SaxonStyleFrame object for it. We need to find a StyleElement
//     // in the style frames stack to be able to ask for global
//     // variables.
//     Stack styleFrames = manager.getStyleFrames();
//     StyleFrame styleFrame = (StyleFrame)styleFrames.get(0);
//     if (styleFrame.getElement() instanceof StyleElement)
//       return styleFrame.getGlobalVariables();
//     else
//       return null;
//   }
}
