/*
    Cocoon1.java

    Wrapper for the Cocoon XML publishing framework to be invoked in
    the command line.

    Author: Ovidiu Predescu <ovidiu@cup.hp.com>
    Date: December 8, 2000

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

import org.apache.cocoon.Cocoon;

public class Cocoon1 
{
  public static String propFilename = null;
  public static String userAgent = null;

  public static void setPropertyFilename(String filename)
  {
    propFilename = filename;
  }

  public static void setUserAgent(String ua)
  {
    userAgent = ua;
  }
  
  public static void invoke(String filename, String outFilename)
  {
    if (propFilename == null || propFilename.equals("")) {
      System.out.println("Please setup the Cocoon properties file in the "
                         + "customization menu");
      return;
    }

    boolean hasUserAgent = userAgent != null && !userAgent.equals("");
    String[] args = new String[hasUserAgent ? 6 : 4];
    int i = 0;

    args[i++] = "-p";
    args[i++] = propFilename;
    if (hasUserAgent) {
      args[i++] = "-A";
      args[i++] = userAgent;
    }
    args[i++] =  filename;
    args[i++] = outFilename;

    try {
      Cocoon.main(args);
    }
    catch (Exception e) {
      System.out.println("Cannot process: " + e);
      e.printStackTrace();
    }
  }
}

// Local Variables:
// c-basic-offset: 2
// End:
