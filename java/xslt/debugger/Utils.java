/*
    Utils.java

    @author: <a href="mailto:ovidiu@cup.hp.com">Ovidiu Predescu</a>
    Date: March 16, 2001

    Copyright (C) 2001 Ovidiu Predescu

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

package xslt.debugger;

import java.util.Vector;
import java.util.StringTokenizer;

public class Utils
{
  public static Vector split(String string, String separator)
  {
    if (string == null || string.equals(""))
      return null;
    
    Vector array = new Vector(2);
    StringTokenizer st = new StringTokenizer(string, separator);

    while (st.hasMoreTokens()) {
      String element = st.nextToken();
      array.add(element);
    }
    return array;
  }
}
