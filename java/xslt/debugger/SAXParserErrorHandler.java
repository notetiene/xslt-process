/*
    SAXParserErrorHandler.java

    @author: <a href="mailto:ovidiu@cup.hp.com">Ovidiu Predescu</a>
    Date: May 17, 2001

 */

package xslt.debugger;
import org.xml.sax.ErrorHandler;
import org.xml.sax.SAXParseException;
import org.xml.sax.SAXException;

/**
 * <code>SAXParserErrorHandler</code> is an implementation of the
 * <code>org.xml.sax.ErrorHandler</code> interface, and is used to
 * report error to an <code>Observer</code> instance.
 *
 * @author <a href="mailto:ovidiu@cup.hp.com">Ovidiu Predescu</a>
 */
public class SAXParserErrorHandler implements ErrorHandler
{
  protected Manager manager;
  
  public SAXParserErrorHandler(Manager manager)
  {
    this.manager = manager;
  }
  
  public void error(SAXParseException ex)
    throws SAXException
  {
    reportError("Error", ex);
  }

  public void warning(SAXParseException ex)
    throws SAXException
  {
    reportError("Warning", ex);
  }

  public void fatalError(SAXParseException ex)
    throws SAXException
  {
    reportError("Fatal error", ex);
  }

  public void reportError(String type, SAXParseException ex)
    throws SAXException
  {
    String message = Utils.uriNoProtocol(ex.getSystemId()) + ":"
      + ex.getLineNumber() + ":";

    if (ex.getColumnNumber() != -1)
      message += ex.getColumnNumber() + ":";

    message += " " + type + ": " + ex.getMessage() + "\n";
    SAXException newEx = new SAXException(message, ex);
    manager.getObserver().caughtException(newEx);
    throw newEx;
  }
}
