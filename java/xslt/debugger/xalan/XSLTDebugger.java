/*
    XSLTDebugger.java

    Author: Ovidiu Predescu <ovidiu@cup.hp.com>
    Date: April 10, 2001

    Copyright (C) 2001 Ovidiu Predescu

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License as
    published by the Free Software Foundation; either version 2 of the
    License, or (at your option) any later version.
   
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    General Public License for more details.
   
    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
    02111-1307, USA.
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
      // Register a TraceListener with a TraceManager associated
      // with the TransformerImpl.
      XalanTraceListener traceListener = new XalanTraceListener();
      TransformerImpl transformerImpl = (TransformerImpl)transformer;
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
