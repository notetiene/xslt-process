/*
    XSLTDebugger.java

    Author: Ovidiu Predescu <ovidiu@cup.hp.com>
    Date: April 10, 2001

 */

package xslt.debugger.xalan;

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
import org.apache.xalan.trace.PrintTraceListener;
import org.apache.xalan.trace.TraceListener;
import xslt.debugger.AbstractXSLTDebugger;
import xslt.debugger.Manager;
import java.io.PrintWriter;

public class XSLTDebugger extends AbstractXSLTDebugger
{
  public XSLTDebugger() {}

  public synchronized void run()
  {
    state = RUNNING;
    notifyAll();

    try {
      System.out.println("Xalan XSLTDebugger starting...");
      
      PrintWriter diagnosticsWriter = new PrintWriter(System.err, true);
      TransformerFactory tFactory
        = new org.apache.xalan.processor.TransformerFactoryImpl();

      PrintTraceListener traceListener
        = new PrintTraceListener(diagnosticsWriter);
      traceListener.m_traceTemplates = true;
      traceListener.m_traceElements = true;
      traceListener.m_traceGeneration = true;
      traceListener.m_traceSelection = true;
      
      //      tFactory.setAttribute(FeatureKeys.TRACE_LISTENER, traceListener);
      //      tFactory.setAttribute(FeatureKeys.LINE_NUMBERING, Boolean.TRUE);

      File inFile = new File(xmlFilename);
      StreamSource in = new StreamSource(xmlFilename);
      StreamResult result = new StreamResult(outStream);

      String media = null, title = null, charset = null;
      Source stylesheet
        = tFactory.getAssociatedStylesheet(in, media, title, charset);
      String stylesheetId = stylesheet.getSystemId();

      Transformer transformer = tFactory.newTransformer(stylesheet);
      if (transformer != null)
        transformer.transform(in, result);

      manager.getObserver().processorFinished();
    }
    catch(Exception e) {
      manager.getObserver().caughtException(e);
    }

    state = NOT_RUNNING;
    notifyAll();
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
