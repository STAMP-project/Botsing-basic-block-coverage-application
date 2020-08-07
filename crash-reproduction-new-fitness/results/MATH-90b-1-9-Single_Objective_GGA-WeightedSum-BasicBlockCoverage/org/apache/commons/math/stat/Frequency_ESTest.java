/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 22:25:08 UTC 2020
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
      Integer integer0 = new Integer((-61));
      frequency0.addValue(integer0);
      int int0 = 3825;
      frequency0.getCumFreq(3825);
      frequency0.getCumFreq((long) 3825);
      frequency0.addValue((Object) integer0);
      Frequency frequency1 = new Frequency();
      frequency1.clear();
      Frequency frequency2 = new Frequency();
      frequency1.getPct((-61));
      frequency2.addValue(integer0);
      char char0 = '\\';
      // Undeclared exception!
      frequency0.addValue('\\');
  }
}
