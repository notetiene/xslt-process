/*
    Manager.java

    Author: Ovidiu Predescu <ovidiu@cup.hp.com>
    Date: March  6, 2001

 */

package xslt.debugger;

import java.lang.ClassNotFoundException;
import java.lang.IndexOutOfBoundsException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Stack;
import java.util.Collection;

/**
 * <code>Manager</code> manages the breakpoints, and local and global
 * variables. It contains an {@link AbstractXSLTDebugger} instance,
 * which is this thread's interface with the real worker thread where
 * the XSLT processor runs.
 *
 * @author <a href="mailto:ovidiu@cup.hp.com">Ovidiu Predescu</a>
 * @see Observer
 * @see AbstractXSLTDebugger
 */
public class Manager
{
  /**
   * <code>styleFrames</code> maintains the stack of frames in the
   * XSLT sheet traversed during the XSLT processing. These are
   * basically the xsl:templates traversed by the processor. The
   * element at index 0 is considered to be the top of the template,
   * or the global scope.
   */
  Stack styleFrames = new Stack();

  /**
   * <code>sourceFrames</code> keeps the stack of frames in the source
   * XML document.
   */
  Stack sourceFrames = new Stack();

  /**
   * <code>breakpoints</code> keeps an association between filename
   * and line number, and the corresponding <code>Breakpoint</code>
   * object.
   */
  HashMap breakpoints = new HashMap();

  /**
   * <code>observer</code> is the instance to be notified when the
   * internal state of the XSLT "program" has been modified.
   */
  Observer observer = null;

  /**
   * <code>debugger</code> is an actual instance of AbstractXSLTDebugger
   * wrapper for the real XSLT processor.
   *
   */
  AbstractXSLTDebugger debugger = null;

  /**
   * <code>worker</code> points to the thread object where the
   * <code>debugger</code> is running.
   *
   */
  Thread worker = null;

  /**
   * Creates a new <code>Manager</code> instance.
   *
   */
  public Manager() {}

  /**
   * Creates a new  XSLT processor to be used for executing the
   * XSLT transformation. <code>processorName</code> is the name of the
   * XSLT processor.
   *
   * @param processorName - <code>String</code>: the name of the XSLT
   * processor
   * @see AbstractXSLTDebugger for more information on what is the
   * class structure of the debugger
   */
  public void setXSLTProcessorType(String processorName)
  {
    try {
      Class debuggerClass = Class.forName("xslt.debugger."
					 + processorName.toLowerCase()
                                         + ".XSLTDebugger");
      debugger = (AbstractXSLTDebugger)debuggerClass.newInstance();
    }
    catch (ClassNotFoundException e) {
      System.out.println("Cannot find the " + processorName
                         + " XSLT processor for debugging!\n" + e);
      System.exit(1);
    }
    catch (Exception e) {
      System.out.println("Cannot create instance of the " + processorName
                         + " XSLT processor!\n" + e);
      System.exit(1);
    }
      
    debugger.setManager(this);
  }

  /**
   * <code>isBreakpoint</code> determines whether a breakpoint is set
   * at a particular location or not.
   *
   * @param filename the <code>String</code> file name
   * @param line the <code>int</code> line number
   * @return true if there's a breakpoint set at this location, false
   * otherwise
   */
  public boolean isBreakpoint(String filename, int line)
  {
    Breakpoint bkp = getBreakpointAt(filename, line);

    if (bkp != null)
      return bkp.isEnabled();
    else
      return false;
  }

  /**
   * Return a <code>Breakpoint</code> instance if there's a breakpoint
   * in the file name, at the line number indicated as arguments, null
   * otherwise.
   *
   * @param filename a <code>String</code> value, representing the
   * file name
   * @param line an <code>int</code> value, representing the line
   * number
   * @return a <code>Breakpoint</code> value or null if no breakpoint
   * is setup at the indicated location
   */
  public Breakpoint getBreakpointAt(String filename, int line)
  {
    String key = filename + ":" + line;
    return (Breakpoint)breakpoints.get(key);
  }
  
  /**
   * Set breakpoint at <code>line</code> in
   * <code>filename</code>. If a breakpoint is already set at that
   * location, the request is ignored.
   * @param filename a <code>String</code> value
   * @param line an <code>int</code> value
   */
  public void setBreakpoint (String filename, int line)
  {
    String key = filename + ":" + line;
    Breakpoint breakpoint = new Breakpoint(filename, line, this);
    breakpoints.put(key, breakpoint);
  }

  /**
   * Remove breakpoint at location. The breakpoint is removed. Doesn't
   * do anything if there's no breakpoint setup at the indicated
   * location.
   *
   * @param filename a <code>String</code> value
   * @param line an <code>int</code> value
   */
  public void removeBreakpoint (String filename, int line)
  {
    String key = filename + ":" + line;
    breakpoints.remove(key);
  }

  /**
   * Enable the breakpoint set up at
   * <code>filename</code>:<code>line</code>. Ignores the request if
   * no breakpoint exists at this location.
   *
   * @param filename a <code>String</code> value
   * @param line an <code>int</code> value
   */
  public void enableBreakpoint(String filename, int line)
  {
    Breakpoint breakpoint = getBreakpointAt(filename, line);
    if (breakpoint != null)
      breakpoint.setEnabled(true);
  }

  /**
   * Disable the breakpoint set up at
   * <code>filename</code>:<code>line</code>. Ignores the request if
   * no breakpoint exists at this location.
   *
   * @param filename a <code>String</code> value
   * @param line an <code>int</code> value
   */
  public void disableBreakpoint(String filename, int line)
  {
    Breakpoint breakpoint = getBreakpointAt(filename, line);
    if (breakpoint != null)
      breakpoint.setEnabled(false);
  }

  /**
   * Return the breakpoints setup in this debugger instance.
   * @return a <code>HashMap</code> with the breakpoints
   */
  public Collection getBreakpoints()
  {
    return breakpoints.values();
  }

  /**
   * Pushes a new source frame on the source frames stack.
   *
   * @param frame a <code>SourceFrame</code> value
   */
  public void pushSourceFrame(SourceFrame frame)
  {
    sourceFrames.push(frame);
  }

  /**
   * <code>peekSourceFrame</code> returns the topmost source frame on
   * the source frames stack.
   *
   * @return a <code>SourceFrame</code> value
   */
  public SourceFrame peekSourceFrame()
  {
    return sourceFrames.size() > 0 ? (SourceFrame)sourceFrames.peek() : null;
  }

  /**
   * <code>popSourceFrame</code> pops the topmost source frame from
   * the source frames stack.
   */
  public void popSourceFrame()
  {
    sourceFrames.pop();
  }

  /**
   * <code>newStyleFrame</code> pushes a StyleFrame instance on the
   * style frames stack and associates it with the corresponding
   * SourceFrame object.
   *
   * @param styleFrame a <code>StyleFrame</code> value
   */
  public void pushStyleFrame(StyleFrame styleFrame)
  {
    SourceFrame sourceFrame = peekSourceFrame();

    styleFrames.push(styleFrame);
    styleFrame.setSourceFrame(sourceFrame);
    if (sourceFrame != null)
      sourceFrame.setStyleFrame(styleFrame);
  }

  /**
   * <code>peekStyleFrame</code> returns the topmost StyleFrame
   * instance on the style frames stack.
   *
   * @return a <code>StyleFrame</code> value
   */
  public StyleFrame peekStyleFrame()
  {
    return styleFrames.size() > 0 ? (StyleFrame)styleFrames.peek() : null;
  }

  /**
   * Return the style frame specified by index in the style frames
   * stack.
   *
   * @param index an <code>int</code> value
   * @return a <code>StyleFrame</code> value
   */
  public StyleFrame peekStyleFrame(int index)
  {
    if (index == -1)
      index = styleFrames.size() - 1;
    
    if (index >= 0 && index < styleFrames.size())
      return (StyleFrame)styleFrames.get(index);
    return null;
  }

  /**
   * <code>popStyleFrame</code> pops the topmost object from the style
   * frames stack.
   */
  public void popStyleFrame()
  {
    styleFrames.pop();
  }

  /**
   * Returns the source frames stack.
   *
   * @return a <code>Stack</code> value
   */
  public Stack getSourceFrames()
  {
    return sourceFrames;
  }
  
  /**
   * Return the XSLT stack frames.
   * @return a <code>Stack</code> value
   */
  public Stack getStyleFrames()
  {
    return styleFrames;
  }

  /**
   * <code>startDebugger</code> starts the XSLT processor under the
   * debugger. The <code>debugger</code> object receives the run()
   * message in a new thread and executes the XSLT processor with the
   * Trace interface setup by the debugger.
   *
   * @param xmlFilename a <code>String</code> value
   * @exception InterruptedException if an error occurs
   * @see AbstractXSLTDebugger
   */
  public synchronized void startDebugger(String xmlFilename)
    throws InterruptedException
  {
    debugger.setXmlFilename(xmlFilename);
    //    debugger.setOutFilename(outputFilename);
    if (!debugger.isStarted()) {
      worker = new Thread(debugger);
      worker.start();
    }
  }
  
  /**
   * <code>getDebugger</code> returns the debugger instance.
   *
   * @return an <code>AbstractXSLTDebugger</code> value
   */
  public AbstractXSLTDebugger getDebugger()
  {
    return debugger;
  }

  /**
   * <code>setObserver</code> sets the observer instance to receive
   * notifications when interesting things happen.
   *
   * @param observer an <code>Observer</code> value
   */
  public void setObserver (Observer observer)
  {
    this.observer = observer;
  }
  
  /**
   * <code>getObserver</code> returns the Observer instance.
   *
   * @return an <code>Observer</code> value
   */
  public Observer getObserver()
  {
    return observer;
  }
}
