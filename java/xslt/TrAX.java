/*
    TrAX.java

    Wrapper for generic TrAX XSLT processors, to be easily invoked
    from BSH.

    Author: Ovidiu Predescu <ovidiu@cup.hp.com>
    Based on initial version from Allan Erskine <a.erskine@cs.ucl.ac.uk>

    Copyright (C) 2001 Allan Erskine and Ovidiu Predescu

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

package xslt;

// Imported TraX classes
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.Transformer;
import javax.xml.transform.Source;
import javax.xml.transform.stream.StreamSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerConfigurationException;

// Imported java classes
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;	
import java.util.HashMap;
import java.net.URLConnection;
import java.net.URL;
import java.net.MalformedURLException;

public class TrAX 
{
  static TransformerFactory tFactory = TransformerFactory.newInstance();
  static HashMap sheetCache = new HashMap();
  
  public static void invoke(String filename, String outFilename)
    throws TransformerException, TransformerConfigurationException
  {
    try {
      File inFile = new File(filename);
      FileOutputStream outStream = new FileOutputStream(outFilename);

      StreamSource in = new StreamSource(inFile);
      StreamResult result = new StreamResult(outStream);

      String media = null, title = null, charset = null;

      Source stylesheet
        = tFactory.getAssociatedStylesheet(in, media, title, charset);
      String stylesheetId = stylesheet.getSystemId();

      // Look for a transformer already in the cache
      XSLTSheetInfo sheetInfo = (XSLTSheetInfo)sheetCache.get(stylesheetId);
      Transformer transformer;
      
      if (sheetInfo == null) {
        sheetInfo = new XSLTSheetInfo(stylesheet);
        sheetCache.put(stylesheetId, sheetInfo);
        transformer = sheetInfo.transformer;
      }
      else {
        // We have already encountered this particular XSLT
        // sheet. Check to see whether it has changed since the last
        // access
        transformer = sheetInfo.getTransformer(stylesheet);
      }

      if (transformer != null)
        transformer.transform(in, result);
      outStream.close();
    }
    catch(Exception e) {
      e.printStackTrace();
    }
  }
}

class XSLTSheetInfo
{
  public Transformer transformer = null;
  public long lastModified = Long.MIN_VALUE;

  public XSLTSheetInfo(Source stylesheet)
  {
    String stylesheetId = stylesheet.getSystemId();

    try {
      lastModified = getLastModified(stylesheetId);
      transformer = TrAX.tFactory.newTransformer(stylesheet);
    }
    catch (TransformerConfigurationException e) {}
  }

  long getLastModified(String stylesheetId)
  {
    long lastModified = Long.MIN_VALUE;
    URL url = null;

    try {
      url = new URL(stylesheetId);
      if (url.getProtocol().equals("file")) {
        File file = new File(url.getFile());
        lastModified = file.lastModified();
      }
      else {
        URLConnection conn = url.openConnection();
        conn.connect();
        lastModified = conn.getLastModified();
      }
    }
    catch (MalformedURLException e) {
      System.err.println("Invalid URL " + url + ": " + e.toString());
    }
    catch (IOException e) {
      System.err.println("Cannot access " + url + ": "+ e.toString());
    }

    return lastModified;
  }

  public Transformer getTransformer(Source stylesheet)
  {
    String stylesheetId = stylesheet.getSystemId();
    long currentTime = Long.MIN_VALUE;

    currentTime = getLastModified(stylesheetId);

    if (currentTime <= lastModified && transformer != null) {
      // The XSLT sheet has not been modified since the last access,
      // return the cached one
      return transformer;
    }

    // The XSLT sheet has been modified, create a new transformer
    // and return it
    try {
      transformer = TrAX.tFactory.newTransformer(stylesheet);
      lastModified = currentTime;
    }
    catch (TransformerConfigurationException e) {
      System.out.println("Could not create transformer for: " + stylesheetId);
    }

    return transformer;
  }
}

// Local Variables:
// c-basic-offset: 2
// End:
