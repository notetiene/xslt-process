/*
    Utils.java

    @author: <a href="mailto:ovidiu@cup.hp.com">Ovidiu Predescu</a>
    Date: March 16, 2001

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
