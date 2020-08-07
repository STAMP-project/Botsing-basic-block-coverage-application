/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 01:29:07 UTC 2020
 */

package org.apache.commons.lang3;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.lang3.StringUtils;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class StringUtils_ESTest extends StringUtils_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      StringUtils.leftPad(" vs ", (-1), " vs ");
      StringUtils.substringBefore("[D-{8t-_wT-:Xp@jHMr", "[D-{8t-_wT-:Xp@jHMr");
      StringUtils.indexOfDifference("xn\"bu^h5eyr7l", " vs ");
      StringUtils.removeEndIgnoreCase("Zpr##b", " vs ");
      StringUtils.containsAny("xn\"bu^h5eyr7l", " vs ");
      StringUtils.substringBetween((String) null, "");
      StringUtils.leftPad(" vs ", (-1));
      StringUtils.lastIndexOf(" vs ", '}', 0);
      StringUtils.overlay("Zpr##b", "Solaris", 0, (-1068));
      String[] stringArray0 = new String[0];
      StringUtils.startsWithAny(" vs ", stringArray0);
      StringUtils.replaceOnce("SolarisZpr##b", "i?nji#}`ex9dD", "Zpr##b");
      StringUtils.replace(" vs ", "ZmBV]O%FNx~V:OQ$v", "}&}>n[px?Rb]h>");
      StringUtils.leftPad("HP-UX", 1359, '}');
      String[] stringArray1 = new String[7];
      stringArray1[0] = "[D-{8t-_wT-:Xp@jHMr";
      stringArray1[1] = "c[>";
      stringArray1[2] = "rlz_7t7a3 /bq";
      stringArray1[3] = "}&}>n[px?Rb]h>";
      stringArray1[4] = "[D-{8t-_wT-:Xp@jHMr";
      stringArray1[5] = "NoSuchMethodException occurred during 1.6 backcompat code";
      stringArray1[6] = null;
      // Undeclared exception!
      StringUtils.replaceEach("c[>", stringArray1, stringArray1);
  }
}
