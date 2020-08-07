/*
 * This file was automatically generated by EvoSuite
 * Fri Jan 17 17:04:16 UTC 2020
 */

package org.apache.commons.lang3.time;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.util.Locale;
import java.util.SimpleTimeZone;
import org.apache.commons.lang3.time.FastDateParser;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class FastDateParser_ESTest extends FastDateParser_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      SimpleTimeZone simpleTimeZone0 = new SimpleTimeZone(120, "6AiE");
      Locale locale0 = FastDateParser.JAPANESE_IMPERIAL;
      FastDateParser fastDateParser0 = new FastDateParser("GMTIET", simpleTimeZone0, locale0);
  }
}
