/*
    SaxonTraceListener.java

    Author: Ovidiu Predescu <ovidiu@cup.hp.com>
    Date: March  6, 2001

 */

package xslt.debugger.saxon;



import com.icl.saxon.Context;
import com.icl.saxon.handlers.NodeHandler;
import com.icl.saxon.om.ElementInfo;
import com.icl.saxon.om.NodeInfo;
import com.icl.saxon.style.StyleElement;
import com.icl.saxon.style.XSLApplyTemplates;
import com.icl.saxon.style.XSLCallTemplate;
import com.icl.saxon.style.XSLAttribute;
import com.icl.saxon.trace.TraceListener;
import xslt.debugger.AbstractXSLTDebugger;
import xslt.debugger.Manager;
import xslt.debugger.SourceFrame;
import xslt.debugger.StyleFrame;

public class SaxonTraceListener implements TraceListener
{
  XSLTDebugger debugger;
  Manager manager;
  String currentFilename = null;
  int currentLine = -1;
  int currentColumn = -1;
  StyleFrame styleFrameToStop = null;
  
  public SaxonTraceListener (XSLTDebugger debugger)
  {
    this.debugger = debugger;
    manager = debugger.getManager();
  }

  // implementation of com.icl.saxon.trace.TraceListener interface

  String indent = "";

  /**
   * Called at start
   */

  public void open()
  {
    System.err.println("<trace>");
  }

  /**
   * Called at end
   */

  public void close()
  {
    System.err.println("</trace>");
  }

  public synchronized void debuggerStopped(NodeInfo element, String message)
  {
    String name = element.getDisplayName();
    String filename = element.getSystemId();
    int line = element.getLineNumber();
    int column = element.getColumnNumber();

    debugger.debuggerStopped(filename, line, column, message);
    currentFilename = filename;
    currentLine = line;
    currentColumn = column;

    // Transform any 'next' command in 'step' if the XSL element
    // is not the apply-templates or call-template instruction
    if (debugger.getAction() == AbstractXSLTDebugger.DO_NEXT) {
      System.out.println("Got a NEXT action for element "
                         + element.getClass());
        
//       if (element instanceof XSLApplyTemplates
//           || element instanceof XSLCallTemplate
//           || element instanceof XSLAttribute) {
      {
        styleFrameToStop = manager.peekStyleFrame();
        System.out.println("Setting up style frame to stop to " + name);
      }
//       else {
//         debugger.setAction(AbstractXSLTDebugger.DO_STEP);
//       }
    }
  }

  /**
   * Called for all top level elements
   */
  public void toplevel(NodeInfo element)
  {
    StyleElement e = (StyleElement)element;
    System.err.println("<Top-level element=\""
                       + e.getDisplayName() + "\" line=\"" + e.getLineNumber()
                       + "\" file=\"" + e.getSystemId()
                       + "\" precedence=\"" + e.getPrecedence()
                       +"\"/>");
  }

  /**
   * Called when a node of the source tree gets processed
   */
  public void enterSource(NodeHandler handler, Context context)
  {
    debugger.checkRequestToStop();

    NodeInfo curr = (NodeInfo)context.getContextNode();
    String name = curr.getDisplayName();
    String filename = curr.getSystemId();
    int line = curr.getLineNumber();
    int column = curr.getColumnNumber();
    System.err.println(indent + "<Source node=\""  + curr.getPath()
		       + "\" line=\"" + curr.getLineNumber()
		       + "\" mode=\"" + getModeName(context) + "\">");
    indent += " ";

    SourceFrame frame = new SourceFrame(name, filename, line, column, manager);
    manager.pushSourceFrame(frame);
  }

  /**
   * Called after a node of the source tree got processed
   */
  public void leaveSource(NodeHandler handler, Context context)
  {
    debugger.checkRequestToStop();

    indent = indent.substring(0, indent.length() - 1);
    System.err.println(indent + "</Source><!-- "  +
		       ((NodeInfo)context.getContextNode()).getPath()
                       + " -->");
    manager.popSourceFrame();
  }

  /**
   * Called when an element of the stylesheet gets processed
   */
  public void enter(NodeInfo element, Context context)
  {
    debugger.checkRequestToStop();

    if (!(element instanceof StyleElement))
      return;

    String name = element.getDisplayName();
    String filename = element.getSystemId();
    int line = element.getLineNumber();
    int column = element.getColumnNumber();

    //     System.err.println(indent
    //                        + "<Instruction " + element.getClass()
    //                        + " element=\"" + name
    //                        + "\" line=\"" + line
    //                        + "\" column=\"" + column
    //                        + "\" file=\"" + filename + "\">");
    indent += " ";

    StyleFrame styleFrame
      = new SaxonStyleFrame(context, (StyleElement)element,
                            name, filename, line, column, manager);
    manager.pushStyleFrame(styleFrame);

    if (manager.isBreakpoint(filename, line)
        && !(filename.equals(currentFilename) && line == currentLine)) {
      debuggerStopped(element, "entering " + element.getDisplayName());
    }
    else {
      switch (debugger.getAction()) {
      case AbstractXSLTDebugger.DO_STEP:
        if (!(filename.equals(currentFilename) && line == currentLine)) {
          // We reached a line different from the last one we were
          // on when the step command was issued. We need to give
          // back the control to the command line
          debuggerStopped(element, "entering " + element.getDisplayName());
        }
        break;

      case AbstractXSLTDebugger.DO_DEFERRED_STOP:
        System.out.println("stopped in deferred stop with element " + name);
        debuggerStopped(element, "entering " + element.getDisplayName());
        break;
        
      default:
        break;
      }
    }
  }

  /**
   * Called after an element of the stylesheet got processed
   */
  public void leave(NodeInfo element, Context context)
  {
    debugger.checkRequestToStop();

    if (!(element instanceof ElementInfo))
      return;

    String name = element.getDisplayName();
    String filename = element.getSystemId();
    int line = element.getLineNumber();
    int column = element.getColumnNumber();

    indent = indent.substring(0, indent.length() - 1);
    //     System.err.println(indent + "</Instruction> <!-- "
    //                        + " element=\"" + element.getDisplayName()
    //                        + "\" line=\"" + line
    //                        + "\" column=\"" + column
    //                        + "\" file=\"" + element.getSystemId() + "\">");

    if (manager.isBreakpoint(filename, line)
        && !(filename.equals(currentFilename) && line == currentLine)) {
      debuggerStopped(element, "leaving " + element.getDisplayName());
    }
    else {
      switch (debugger.getAction()) {
      case AbstractXSLTDebugger.DO_STEP:
        if (!(filename.equals(currentFilename) && line == currentLine)) {
          // We reached a line different from the last one we were
          // on when the step command was issued. We need to give
          // back the control to the command line
          debuggerStopped(element, "leaving " + element.getDisplayName());
        }
        break;

      case AbstractXSLTDebugger.DO_NEXT:
        if (styleFrameToStop != null
            && styleFrameToStop == manager.peekStyleFrame()) {
          // We reached the end of the element after which we have to
          // stop. Set the action to STEP so that we stop right after this
          // node and continue
          debugger.setAction(AbstractXSLTDebugger.DO_DEFERRED_STOP);
          System.out.println("Leaving " + name
                             + ", setting up deferred breakpoint");
          styleFrameToStop = null;
        }
        break;
      }
    }
    manager.popStyleFrame();
  }

  String getModeName(Context context)
  {
    int nameCode = context.getMode().getNameCode();

    if (nameCode == -1)
      return "*default*";
    else
      return context.getController().getNamePool().getDisplayName(nameCode);
  }
}
