/*
    EmacsObserver.java

    @author: <a href="mailto:ovidiu@cup.hp.com">Ovidiu Predescu</a>
    Date: March 29, 2001

 */

package xslt.debugger.cmdline;

import java.net.Socket;
import java.net.ServerSocket;
import java.net.InetAddress;
import java.io.OutputStream;

import xslt.debugger.Observer;
import xslt.debugger.AbstractXSLTDebugger;

public class EmacsObserver implements Observer
{
  Controller controller;
  
  public EmacsObserver(Controller controller)
  {
    this.controller = controller;

    // Create a server socket, send the port number back to Emacs, and
    // wait for Emacs to connect to it. Upon connection, setup an
    // output stream on the client socket and use it to serialize the
    // XSLT processing results to Emacs.
    try {
      InetAddress localhost = InetAddress.getByName("localhost");
      ServerSocket serverSocket = new ServerSocket(0, 5, localhost);
      int port = serverSocket.getLocalPort();
      System.out.println("<<(xslt-process-set-output-port " + port + ")>>");
      Socket clientSocket = serverSocket.accept();
      OutputStream outputStream = clientSocket.getOutputStream();
      AbstractXSLTDebugger debugger = controller.getDebugger();
      debugger.setOutStream(outputStream);
      serverSocket.close();
    }
    catch (Exception e) {
      caughtException(e);
    }
  }

  public void debuggerProcessStarted()
  {
    System.out.println("<<(xslt-process-debugger-process-started)>>");
  }

  /**
   * Invoked by the debugger to print a descriptive message showing
   * where the debugger stopped.
   *
   * @param filename a <code>String</code> value
   * @param line an <code>int</code> value
   * @param column an <code>int</code> value
   * @param message a <code>String</code> value, additional
   * informative message
   */
  public void debuggerStopped(String filename,
                              int line,
                              int column,
                              String message)
  {
    if (filename.startsWith("file:"))
      filename = filename.substring(5);
    System.out.println("<<(xslt-process-debugger-stopped-at "
                       + "\"" + filename
                       + "\" " + line
                       + " " + column
                       + " \"" + message + "\")>>");
  }

  public void breakpointSetAt(String filename, int line) {}
  
  public void breakpointDeletedAt(String filename, int line) {}
  
  public void breakpointEnabledAt(String filename, int line) {}
  
  public void breakpointDisabledAt(String filename, int line) {}

  /**
   *
   */
  public void stackChanged()
  {
    // TODO: implement this xslt.debugger.Observer method
  }

  /**
   *
   */
  public void frameChanged()
  {
    // TODO: implement this xslt.debugger.Observer method
  }

  public void processorFinished()
  {
    System.out.println("<<(xslt-process-processor-finished)>>");
  }

  public void caughtException(Exception e)
  {
    System.out.println("<<(xslt-process-report-error \""
                       + e.getMessage()
                       + "\")>>");
    
  }
}
