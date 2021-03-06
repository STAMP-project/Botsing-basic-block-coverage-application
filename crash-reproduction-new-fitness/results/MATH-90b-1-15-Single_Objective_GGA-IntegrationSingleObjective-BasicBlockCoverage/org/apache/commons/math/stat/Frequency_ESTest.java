/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 23:05:47 UTC 2020
 */

package org.apache.commons.math.stat;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.stat.Frequency;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class Frequency_ESTest extends Frequency_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      Frequency frequency0 = new Frequency();
      Integer integer0 = new Integer(0);
      frequency0.getCumPct((Object) integer0);
      Object object0 = new Object();
      frequency0.getCumPct(object0);
      frequency0.addValue((Object) integer0);
      frequency0.getCount((long) 0);
      frequency0.addValue((-1403));
      frequency0.getCumPct(495L);
      frequency0.toString();
      frequency0.getCumFreq((-1403));
      frequency0.clear();
      frequency0.clear();
      frequency0.getCumPct('n');
      Object object1 = new Object();
      frequency0.getCumFreq(object1);
      frequency0.getCount(1L);
      frequency0.addValue((-1403));
      Object object2 = new Object();
      // Undeclared exception!
      frequency0.addValue(object2);
  }
}
