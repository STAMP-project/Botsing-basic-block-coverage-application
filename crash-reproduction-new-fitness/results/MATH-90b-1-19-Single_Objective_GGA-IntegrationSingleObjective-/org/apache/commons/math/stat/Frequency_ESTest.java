/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 03:34:22 UTC 2020
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
      Frequency frequency1 = new Frequency();
      frequency1.valuesIterator();
      frequency0.getCumFreq('(');
      frequency0.clear();
      frequency1.getCount('y');
      // Undeclared exception!
      frequency1.addValue((Object) frequency0);
  }
}
