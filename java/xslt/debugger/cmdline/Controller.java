/*
    Controller.java

    Author: Ovidiu Predescu <ovidiu@cup.hp.com>
    Date: March  7, 2001

 */

package xslt.debugger.cmdline;

import java.util.Vector;
import java.util.Stack;
import java.util.Iterator;
import java.util.HashMap;
import java.util.Collection;
import java.util.ArrayList;
import java.text.MessageFormat;
import java.lang.reflect.Method;
import java.lang.NoSuchMethodException;
import java.io.InputStreamReader;
import java.io.IOException;
import java.io.File;
import java.io.BufferedReader;

import xslt.debugger.Variable;
import xslt.debugger.Value;
import xslt.debugger.Utils;
import xslt.debugger.StyleFrame;
import xslt.debugger.SourceFrame;
import xslt.debugger.Observer;
import xslt.debugger.Observer;
import xslt.debugger.Manager;
import xslt.debugger.Breakpoint;
import xslt.debugger.AbstractXSLTDebugger;
import xslt.debugger.Type;

/**
 * This is the public class used by the command line interface to
 * interact with the XSLT debugger.
 *
 * @author <a href="mailto:ovidiu@cup.hp.com">Ovidiu Predescu</a>
 * @see xslt.debugger.Manager
 * @see xslt.debugger.AbstractXSLTDebugger
 */
public class Controller
{
  static final String breakpointUsage = "Usage: b filename line";
  static final String deleteBreakpointUsage = "Usage: d <breakpoint number>";
  static final String disableBreakpointUsage
    = "Usage: dis (<breakpoint number> | filename lineno)";
  static final String enableBreakpointUsage
    = "Usage: ena (<breakpoint number> | filename lineno)";
  static final String runUsage = "Usage: r filename";
  static final String setSourceFrameUsage = "Usage: sf <framenumber>";
  static final String setStyleFrameUsage = "Usage: xf <framenumber>";
  static final String printLocalVariableUsage = "Usage: pl <name>";
  static final String printGlobalVariableUsage = "Usage: pg <name>";

  Manager manager = new Manager();
  AbstractXSLTDebugger debugger;
  HashMap commands = new HashMap();
  int currentSourceFrame = -1;
  int currentStyleFrame = -1;
  Observer observer = null;

  public Controller(String processorName, Observer observer)
  {
    commands.put("h", getMethod("help"));
    commands.put("help", getMethod("help"));
    commands.put("b", getMethod("setBreakpoint"));
    commands.put("d", getMethod("deleteBreakpoint"));
    commands.put("dis", getMethod("disableBreakpoint"));
    commands.put("ena", getMethod("enableBreakpoint"));
    commands.put("lb", getMethod("listBreakpoints"));
    commands.put("r", getMethod("runXSLTProcessor"));
    commands.put("s", getMethod("doStep"));
    commands.put("n", getMethod("doNext"));
    commands.put("c", getMethod("continueProcessing"));
    commands.put("lv", getMethod("showLocalVariables"));
    commands.put("gv", getMethod("showGlobalVariables"));
    commands.put("pl", getMethod("printLocalVariable"));
    commands.put("pg", getMethod("printGlobalVariable"));
    commands.put("sf", getMethod("setCurrentSourceFrame"));
    commands.put("xf", getMethod("setCurrentStyleFrame"));
    commands.put("sbt", getMethod("showSourceFrames"));
    commands.put("xbt", getMethod("showStyleFrames"));
    commands.put("stop", getMethod("stopXSLTProcessing"));
    commands.put("q", getMethod("quit"));
    this.observer = observer;
    manager.setObserver(this.observer);
    manager.setXSLTProcessorType(processorName);
    debugger = manager.getDebugger();
  }

  public void help(Vector args)
  {
    System.out.println("Known commands:\n\n"
                       + "help or h\t shows this message\n"
                       + "b\t set breakpoint\n"
                       + "d\t delete breakpoint\n"
                       + "dis\t disable breakpoint\n"
                       + "ena\t enable breakpoint\n"
                       + "lb\t list breakpoints\n"
                       + "r\t run XSLT processor\n"
                       + "s\t step\n"
                       + "n\t next\n"
                       + "c\t continue\n"
                       + "lv\t show local variables\n"
                       + "gv\t show global variables\n"
                       + "pl\t print value of local variable\n"
                       + "pg\t print value of global variable\n"
                       + "sf\t set current source frame\n"
                       + "xf\t set current style frame\n"
                       + "xbt\t show XSLT stack frames\n"
                       + "stop\t stop (useful with long processings)\n"
                       + "q\t quit\n");
  }

  public static void main(String[] args)
  {
    Controller controller = new Controller("Saxon", new CmdLineObserver());
    controller.mainLoop();
  }

  public Method getMethod(String mthName)
  {
    Class[] args = { Vector.class };
    
    try {
      return this.getClass().getMethod(mthName, args);
    }
    catch (NoSuchMethodException e) {
      System.out.println("No such method: " + mthName + ": " + e);
    }
    return null;
  }

  /**
   * <code>setBreakpoint</code> sets a breakpoint at a filename and
   * line number indicated as arguments. This method is invoked
   * whenever the user runs the 'b' command.
   *
   * @param args a <code>Vector</code> value containing the arguments
   * of the command
   * @exception NumberFormatException if an error occurs processing
   * the line number, which should be a positive integer.
   */
  public void setBreakpoint(Vector args)
    throws NumberFormatException, IOException
  {
    String filename;
    int line;
    
    if (args.size() != 3) {
      System.out.println(breakpointUsage);
      return;
    }

    filename = getAbsoluteFilename((String)args.get(1));
    line = getPositiveInteger((String)args.get(2), breakpointUsage, true);

    manager.setBreakpoint(filename, line);
    if (observer != null)
      observer.breakpointSetAt(filename, line);
  }

  /**
   * Remove breakpoint at filename and line indicated as arguments.
   *
   * @param args a <code>Vector</code> value containing the arguments
   * of the command.
   * @exception NumberFormatException if an error occurs
   * @exception IOException if an error occurs
   */
  public void deleteBreakpoint(Vector args)
    throws NumberFormatException, IOException
  {
    if (args.size() < 2 && args.size() > 3) {
      System.out.println(deleteBreakpointUsage);
      return;
    }

    String filename;
    int line;
    int index;

    // Check for the first variant: "d <breakpoint number>"
    index = getPositiveOrZeroInteger((String)args.get(1),
                                     deleteBreakpointUsage, false);
    if (index >= 0) {
      Breakpoint bkp = getBreakpointNo(index);
      if (bkp == null)
        return;
      filename = bkp.getFilename();
      line = bkp.getLine();
    }
    else {
      filename = getAbsoluteFilename((String)args.get(1));
      line = getPositiveInteger((String)args.get(2),
                                deleteBreakpointUsage, true);
    }

    manager.removeBreakpoint(filename, line);
    if (observer != null)
      observer.breakpointDeletedAt(filename, line);
  }

  /**
   * Disables the breakpoint indicated by the number passed as
   * argument. The number is an index in the list of breakpoints as
   * returned by listBreakpoints.
   *
   * @param args a <code>Vector</code> value
   */
  public void disableBreakpoint(Vector args)
    throws IOException
  {
    if (args.size() < 2 && args.size() > 3) {
      System.out.println(disableBreakpointUsage);
      return;
    }

    String filename;
    int line;
    int index;

    // Check for the first variant: "dis <breakpoint number>"
    index = getPositiveOrZeroInteger((String)args.get(1),
                                     disableBreakpointUsage, false);
    Breakpoint bkp = null;
    
    if (index >= 0)
      bkp = getBreakpointNo(index);
    else {
      filename = getAbsoluteFilename((String)args.get(1));
      line = getPositiveInteger((String)args.get(2),
                                disableBreakpointUsage, true);
      bkp = manager.getBreakpointAt(filename, line);
    }

    if (bkp == null)
      return;

    bkp.setEnabled(false);
    if (observer != null)
      observer.breakpointDisabledAt(bkp.getFilename(), bkp.getLine());
  }

  /**
   * Enables the breakpoint indicated by the number passed as
   * argument. The number is an index in the list of breakpoints as
   * returned by listBreakpoints.
   *
   * @param args a <code>Vector</code> value
   */
  public void enableBreakpoint(Vector args)
    throws IOException
  {
    if (args.size() < 2 && args.size() > 3) {
      System.out.println(enableBreakpointUsage);
      return;
    }

    String filename;
    int line;
    int index;

    // Check for the first variant: "ena <breakpoint number>"
    index = getPositiveOrZeroInteger((String)args.get(1),
                                     enableBreakpointUsage, false);
    Breakpoint bkp = null;
    
    if (index >= 0)
      bkp = getBreakpointNo(index);
    else {
      filename = getAbsoluteFilename((String)args.get(1));
      line = getPositiveInteger((String)args.get(2),
                                enableBreakpointUsage, true);
      bkp = manager.getBreakpointAt(filename, line);
    }

    if (bkp == null)
      return;

    bkp.setEnabled(true);
    if (observer != null)
      observer.breakpointEnabledAt(bkp.getFilename(), bkp.getLine());
  }

  /**
   * Common method for removing, enabling and disabling
   * breakpoints. Given an index in the list of breakpoints as
   * returned by listBreakpoints, returns the Breakpoint instance.
   *
   * @param index an <code>int</code> value
   * @return a <code>Breakpoint</code> value
   */
  protected Breakpoint getBreakpointNo(int index)
  {
    Collection breakpoints = manager.getBreakpoints();
    
    if (index >= breakpoints.size()) {
      System.out.println("Breakpoint number greater than number of "
                         + "breakpoints: " + breakpoints.size());
      return null;
    }

    Object[] bkpArray = breakpoints.toArray();
    Breakpoint bkp = (Breakpoint)bkpArray[index];
    return bkp;
  }

  /**
   * Generates a list of breakpoints to the output. If the breakpoint
   * is disabled a 'D' character indicates it.
   *
   * @param args a <code>Vector</code> value
   */
  public void listBreakpoints(Vector args)
  {
    Collection breakpoints = manager.getBreakpoints();
    Iterator iter = breakpoints.iterator();
    int i = 0;

    while (iter.hasNext()) {
      Breakpoint bkp = (Breakpoint)iter.next();
      String filename = bkp.getFilename();
      int line = bkp.getLine();
      Object[] msgArgs = {
        new Integer(i),
        bkp.isEnabled() ? " " : "D",
        filename,
        new Integer(line)
      };
      String fmt = MessageFormat.format("{0,number,##} {1} {2} {3,number}",
                                        msgArgs);
      System.out.println(fmt);
      i++;
    }
  }

  /**
   * <code>runXSLTProcessor</code> starts the execution of the XSLT
   * processor on the XML filename specified as argument. The
   * processor will stop when it hits a breakpoint, so make sure you
   * setup breakpoints before invoking it. If the processing takes a
   * long time you can stop it using the stopXSLTProcessing function
   * (the 'stop' command).
   *
   * @param args a <code>Vector</code> value containing the argument
   * of the command.
   */
  public void runXSLTProcessor(Vector args)
    throws IOException, InterruptedException
  {
    if (args.size() != 2) {
      System.out.println(runUsage);
      return;
    }

    // Clear the current source and style frames
    currentSourceFrame = -1;
    currentStyleFrame = -1;
    
    if (debugger.isStarted()) {
      System.out.print("XSLT processor already running, "
                       + "do you want to restart? (y/n)");
      int resp = 0;
      try {
        resp = System.in.read();
      }
      catch (IOException e) {
      }
      if (!(resp == 'y' || resp == 'Y'))
        debugger.stopProcessing();
    }

    manager.startDebugger(getAbsoluteFilename((String)args.get(1)));
  }

  public void doStep(Vector args)
  {
    // Clear the current source and style frames
    currentSourceFrame = -1;
    currentStyleFrame = -1;
    
    debugger.doStep();
  }

  public void doNext(Vector args)
  {
    // Clear the current source and style frames
    currentSourceFrame = -1;
    currentStyleFrame = -1;
    
    debugger.doNext();
  }

  public void continueProcessing (Vector args)
  {
    // Clear the current source and style frames
    currentSourceFrame = -1;
    currentStyleFrame = -1;
    
    debugger.doContinue();
  }

  /**
   * Sets the current source frame. This could modify the style frame
   * pointer.
   *
   * @param args a <code>Vector</code> value
   */
  public void setCurrentSourceFrame(Vector args)
  {
    if (args.size() != 2) {
      System.out.println(setSourceFrameUsage);
      return;
    }

    Stack sourceFrames = manager.getSourceFrames();
    int frameNo = getPositiveOrZeroInteger((String)args.get(1),
                                           setSourceFrameUsage, true);
    if (frameNo >= sourceFrames.size()) {
      System.out.println("Requested frame number bigger than number of "
                         + "frames: " + sourceFrames.size());
      return;
    }

    currentSourceFrame = frameNo;
    SourceFrame sourceFrame
      = (SourceFrame)sourceFrames.get(currentSourceFrame);
    StyleFrame styleFrame = sourceFrame.getStyleFrame();
    Stack styleFrames = manager.getStyleFrames();
    currentStyleFrame = styleFrames.indexOf(styleFrame);
  }

  public void setCurrentStyleFrame(Vector args)
  {
    if (args.size() != 2) {
      System.out.println(setStyleFrameUsage);
      return;
    }

    Stack styleFrames = manager.getStyleFrames();
    int frameNo = getPositiveOrZeroInteger((String)args.get(1),
                                           setStyleFrameUsage, true);
    if (frameNo >= styleFrames.size()) {
      System.out.println("Requested frame number bigger than number of "
                         + "frames: " + styleFrames.size());
      return;
    }

    currentStyleFrame = frameNo;
    StyleFrame styleFrame
      = (StyleFrame)styleFrames.get(currentStyleFrame);
    SourceFrame sourceFrame = styleFrame.getSourceFrame();
    Stack sourceFrames = manager.getSourceFrames();
    currentSourceFrame = sourceFrames.indexOf(sourceFrame);
  }
  
  public void showSourceFrames(Vector args)
  {
    Stack sourceFrames = manager.getSourceFrames();

    if (currentSourceFrame == -1)
      currentSourceFrame = sourceFrames.size() - 1;
    
    for (int i = 0; i < sourceFrames.size(); i++) {
      SourceFrame sourceFrame = (SourceFrame)sourceFrames.get(i);
      Object[] msgArgs = {
        new Integer(i),
        currentSourceFrame == i ? "*" : " ",
        sourceFrame.getName() 
      };
      String line = MessageFormat.format("{0,number,##} {1} {2}", msgArgs);
      System.out.println(line);
    }
  }

  public void showStyleFrames(Vector args)
  {
    Stack styleFrames = manager.getStyleFrames();

    if (currentStyleFrame == -1)
      currentStyleFrame = styleFrames.size() - 1;
    
    for (int i = 0; i < styleFrames.size(); i++) {
      StyleFrame styleFrame = (StyleFrame)styleFrames.get(i);
      Object[] msgArgs = {
        new Integer(i),
        currentStyleFrame == i ? "*" : " ",
        styleFrame.getName() 
      };
      String line = MessageFormat.format("{0,number,##} {1} {2}", msgArgs);
      System.out.println(line);
    }
  }

  /**
   * Shows the names of the local variables in the current style
   * frame. To view the value of a local variable use the 'pl'
   * command, which is bound to printLocalVariable.
   *
   * @param args a <code>Vector</code> value
   */
  public void showLocalVariables(Vector args)
  {
    StyleFrame styleFrame = manager.peekStyleFrame(currentStyleFrame);
    ArrayList variables = styleFrame.getLocalVariables();
    showVariables(variables);
  }

  /**
   * Display the global variables. To view the value of a global
   * variable use the 'pg' command, bound to the printGlobalVariable
   * method.
   *
   * @param args a <code>Vector</code> value
   */
  public void showGlobalVariables(Vector args)
  {
    ArrayList variables = debugger.getGlobalVariables();
    showVariables(variables);
  }

  /**
   * Common method used by both showLocalVariables and
   * showGlobalVariables methods to display the name of their
   * respective variables.
   *
   * @param variables an <code>ArrayList</code> value
   */
  protected void showVariables(ArrayList variables)
  {
    if (variables == null)
      return;
    
    for (int i = 0; i < variables.size(); i++) {
      Variable var = (Variable)variables.get(i);
      String name = var.getName();
      System.out.println(name);
    }
  }

  /**
   * Prints the value of a local variable.
   *
   * @param args a <code>Vector</code> value
   */
  public void printLocalVariable(Vector args)
  {
    if (args.size() != 2) {
      System.out.println(printLocalVariableUsage);
      return;
    }

    String name = (String)args.get(1);

    StyleFrame styleFrame = manager.peekStyleFrame(currentStyleFrame);
    if (styleFrame == null) {
      System.out.println("No such variable: " + name);
      return;
    }

    Value value = null;
    try {
      value = styleFrame.getValueOfLocalVariable(name);
    }
    catch (NoSuchFieldException e) {
      System.out.println("No such variable: " + name);
      return;
    }

    if (value == null)
      System.out.println(name + ": null");
    else {
      String stringValue = value.getValue();
      Type type = value.getType();
      String typeName = (type == null ? "unknown" : type.getTypeName());
      System.out.println(name + ": " + typeName + "\n" + stringValue);
    }
  }
  
  /**
   * Prints the value of a global variable.
   *
   * @param args a <code>Vector</code> value
   */
  public void printGlobalVariable(Vector args)
  {
    if (args.size() != 2) {
      System.out.println(printGlobalVariableUsage);
      return;
    }

    String name = (String)args.get(1);

    Value value = null;
    try {
      value = debugger.getValueOfGlobalVariable(name);
    }
    catch (NoSuchFieldException e) {
      System.out.println("No such variable: " + name);
      return;
    }

    if (value == null)
      System.out.println(name + ": null");
    else {
      String stringValue = value.getValue();
      Type type = value.getType();
      String typeName = (type == null ? "unknown" : type.getTypeName());
      System.out.println(name + ": " + typeName + "\n" + stringValue);
    }
  }
  
  public void quit(Vector args)
  {
    System.out.println("Quit");
    System.exit(0);
  }

  public void stopXSLTProcessing(Vector args)
    throws InterruptedException
  {
    debugger.stopProcessing();
  }

  protected String getAbsoluteFilename(String filename)
    throws IOException
  {
    File file = new File(filename);
    String canonicalName;
    
    try {
      canonicalName = file.getCanonicalPath();
    }
    catch (IOException e) {
      System.out.println("Error obtaining the absolute filename: " + e);
      throw e;
    }

    if (!file.exists()) {
      System.out.println("Filename doesn't exists: " + canonicalName);
      throw new IOException();
    }

    return "file:" + canonicalName;
  }

  protected int getPositiveInteger(String arg, String usage, boolean showError)
    throws NumberFormatException
  {
    int value = getPositiveOrZeroInteger(arg, usage, showError);
    if (value == 0 && showError) {
      System.out.println("Line number must be integer greater than 0, got "
                         + arg + ".");
      System.out.println(usage);
      throw new NumberFormatException();
    }
    return value;
  }

  protected int getPositiveOrZeroInteger(String arg, String usage,
                                         boolean showError)
    throws NumberFormatException
  {
    int value = -1;
    
    try {
      value = Integer.parseInt(arg);
      if (value < 0)
        throw new NumberFormatException();
    }
    catch (NumberFormatException e) {
      if (showError) {
        System.out.println("Line number must be integer greater or equal to 0,"
                           + " got " + arg + ".");
        System.out.println(usage);
        throw e;
      }
    }

    return value;
  }

  public void mainLoop()
  {
    InputStreamReader sin = new InputStreamReader(System.in);
    BufferedReader stdin = new BufferedReader(sin);

    while (true) {
      String line;

      // Read a command from the input
      System.out.print("xslt> ");
      System.out.flush();
      try {
        line = stdin.readLine();
      }
      catch (IOException e) {
        return;
      }

      if (line == null) {
        System.out.println("\nQuit");
        System.exit(0);
      }
      if (line.equals(""))
        continue;
      
      Vector args = Utils.split(line, " ");
      String cmd = (String)args.get(0);
      Method method = (Method)commands.get(cmd);
      if (method == null) {
        System.out.println("No such command '" + cmd + "'.");
        continue;
      }

      Object[] mthArguments = { args };
      try {
        method.invoke(this, mthArguments);
      }
      catch (NumberFormatException e) {
        // Message should have been provided
      }
      catch (Exception e) {
        System.out.println("Error invoking method '"
                           + method.getName() + "': " + e);
      }
    }
  }
}
