/*
    Xalan1.java

    Wrapper for the Xalan 1.x processor to be easily invoked from BSH.

    Author: Ovidiu Predescu <ovidiu@cup.hp.com>
    Date: December 6, 2000

    Copyright (C) 2000 Ovidiu Predescu

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

/*
    This file contains portions of code from the Xalan 1.2
    distribution, released under the Apache Software License, Version
    1.1, copyrighted as follows:

    Copyright (c) 1998-1999 Lotus Corporation, Inc. All Rights
    Reserved.  This software is provided without a warranty of any
    kind.
*/

package xslt;

import org.apache.xalan.xslt.XSLTProcessorFactory;
import org.apache.xalan.xslt.XSLTProcessor;
import org.apache.xalan.xslt.XSLTInputSource;
import org.w3c.dom.Node;
import org.w3c.dom.NamedNodeMap;
import org.apache.xalan.xslt.XSLTResultTarget;
import org.w3c.dom.ProcessingInstruction;
import java.util.Hashtable;
import java.util.StringTokenizer;
import java.net.URL;
import java.io.InputStream;
import java.io.FileOutputStream;

public class Xalan1 
{
  public static void invoke(String filename, String outFilename)
  {
    try {
      XSLTProcessor processor = XSLTProcessorFactory.getProcessor();
      URL top = new URL("file:/");
      URL xmlDocURL = new URL(top, "file:" + filename);

      XSLTInputSource xmlDoc = new XSLTInputSource(xmlDocURL.openStream());
      Node document = processor.getSourceTreeFromInput(xmlDoc);
      String xslDocFilename = null;

      /* Search for a processing instruction that identifies the
       * stylesheet to be used and obtain the XSLT sheet to be applied
       * on the document. */
      for (Node child = document.getFirstChild();
           child != null;
           child = child.getNextSibling()) {
        if (child.getNodeType() == Node.PROCESSING_INSTRUCTION_NODE) {
          ProcessingInstruction pi = (ProcessingInstruction)child;
          if (pi.getNodeName().equals("xml-stylesheet")) {
            PIA pia = new PIA(pi);

            String type = pia.getAttribute("type");
              
            if (type == null || !type.equals("text/xsl"))
              continue;

            String href = pia.getAttribute("href");

            if (href == null)
              continue;
            xslDocFilename = href;
            break;
          }
        }
      }

      if (xslDocFilename == null) {
        System.out.println("Cannot process " + filename
                           + ":  no stylesheet found!");
        return;
      }

      // Create the XSL document URI relative to the one of the XML
      // document
      URL xslDocURL = new URL(xmlDocURL, xslDocFilename);      
      InputStream xslDocStream = xslDocURL.openStream();
      XSLTInputSource xslDoc = new XSLTInputSource(xslDocStream);
      FileOutputStream out = new FileOutputStream(outFilename);
      // Create a new instance of xmlDoc as the one used to parse the
      // original document cannot be passed to the process() method
      xmlDoc = new XSLTInputSource(xmlDocURL.openStream());
      // Finally process the XML document through the XSLT processor
      processor.process(xmlDoc, xslDoc, new XSLTResultTarget(out));
      out.close();
    }
    catch(Exception e) {
      System.out.println("Cannot process: " + e);
    }
  }
}

/* Class stolen from the Xalan sample Servlet/DefaultApplyXSL.java */

/**
 * Parses a processing instruction's (PI) attributes for easy
 * retrieval.
 */
class PIA
{
  private Hashtable piAttributes = null;

  /**
   * Constructor.
   * @param pi The processing instruction whose attributes are to be parsed
   */
  PIA(ProcessingInstruction pi)
  {
    piAttributes = new Hashtable();
    StringTokenizer tokenizer = new StringTokenizer(pi.getNodeValue(), "=\"");
    while(tokenizer.hasMoreTokens()) {
      piAttributes.put(tokenizer.nextToken().trim(),
                       tokenizer.nextToken().trim());
    }
  }

  /**
   * Returns value of specified attribute.
   *  @param name Attribute name
   *  @return Attribute value, or null if the attribute name does not exist
   */
  String getAttribute(String name)
  {
    return (String) piAttributes.get(name);
  }
}

// Local Variables:
// c-basic-offset: 2
// End:
