/*
    Saxon.java

    Wrapper for the Saxon processor to be easily invoked from BSH.

    Author: Ovidiu Predescu <ovidiu@cup.hp.com>
    Date: December 2, 2000

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

package xslt;

import java.io.InputStream;
import java.net.URL;
import com.icl.saxon.StyleSheet;
import org.xml.sax.InputSource;
import com.icl.saxon.ParameterSet;
import com.icl.saxon.trax.Processor;
import com.icl.saxon.trax.Templates;
import com.icl.saxon.trax.Transformer;
import com.icl.saxon.trax.serialize.Method;
import com.icl.saxon.trax.serialize.OutputFormat;
import java.io.FileOutputStream;
import com.icl.saxon.trax.Result;

public class Saxon 
{
  public static void invoke(String filename, String outFilename)
  {
    try {
      URL top = new URL("file:/");
      URL url = new URL(top, "file:" + filename);
      filename = url.toExternalForm();
      Processor processor = Processor.newInstance("xslt");
      InputSource docSource = new InputSource(filename);
      InputSource[] sources
        = processor.getAssociatedStylesheets(docSource, null, null, null);

      if (sources == null) {
        System.err.println("No stylesheet found");
        return;
      }

      FileOutputStream out = new FileOutputStream(outFilename);
      Result result = new Result(out);
      Templates templates = processor.processMultiple(sources);
      Transformer transformer = templates.newTransformer();
      OutputFormat format = new OutputFormat();
      format.setMethod(Method.XML);
      format.setIndenting(true);

      transformer.setOutputFormat(format);
      transformer.transform(docSource, result);
      out.close();
    }
    catch (Exception e) {
      System.err.println("Cannot process: " + e);
    }
  }
}

// Local Variables:
// c-basic-offset: 2
// End:
