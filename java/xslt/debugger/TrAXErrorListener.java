/*
    TrAXErrorListener.java

    @author: <a href="mailto:ovidiu@cup.hp.com">Ovidiu Predescu</a>
    Date: May 17, 2001

 */

package xslt.debugger;
import javax.xml.transform.ErrorListener;
import javax.xml.transform.TransformerException;
import javax.xml.transform.SourceLocator;
import javax.xml.transform.TransformerConfigurationException;

/**
 * Implementation of the
 * <code>java.xml.transform.ErrorListener</code> interface in the
 * TrAX interface, and is used to report error to an
 * <code>Observer</code> instance.
 *
 * @author <a href="mailto:ovidiu@cup.hp.com">Ovidiu Predescu</a>
 */
public class TrAXErrorListener implements ErrorListener
{
  protected Manager manager;
  
  public TrAXErrorListener(Manager manager)
  {
    this.manager = manager;
  }
    
  public void error(TransformerException ex)
    throws TransformerException
  {
    reportError("Error", ex);
  }

  public void warning(TransformerException ex)
    throws TransformerException
  {
    reportError("Warning", ex);
  }

  public void fatalError(TransformerException ex)
    throws TransformerException
  {
    reportError("Fatal error", ex);
  }

  public void reportError(String type, TransformerException ex)
    throws TransformerException
  {
    SourceLocator locator = ex.getLocator();
    if (locator != null) {
      String message = Utils.uriNoProtocol(locator.getSystemId()) + ":"
        + locator.getLineNumber() + ":";

      if (locator.getColumnNumber() != -1)
        message = message + locator.getColumnNumber() + ":";
      message = message + " " + type + ": " + ex.getMessage() + "\n";
      
      ex = new TransformerConfigurationException(message);
    }
      
    manager.getObserver().caughtException(ex);
    throw ex;
  }
}
