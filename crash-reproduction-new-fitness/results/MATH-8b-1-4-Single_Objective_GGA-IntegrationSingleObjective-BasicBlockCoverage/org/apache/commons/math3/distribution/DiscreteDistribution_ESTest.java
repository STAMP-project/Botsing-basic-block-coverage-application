/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 23:07:13 UTC 2020
 */

package org.apache.commons.math3.distribution;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.util.LinkedList;
import org.apache.commons.math3.distribution.DiscreteDistribution;
import org.apache.commons.math3.random.ISAACRandom;
import org.apache.commons.math3.util.Pair;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class DiscreteDistribution_ESTest extends DiscreteDistribution_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      ISAACRandom iSAACRandom0 = new ISAACRandom();
      Integer[] integerArray0 = new Integer[10];
      Double double0 = new Double(2);
      Double double1 = new Double(2);
      Double.sum(2, 2);
      Pair<Object, Double> pair0 = new Pair<Object, Double>(double0, double1);
      LinkedList<Pair<Object, Double>> linkedList0 = new LinkedList<Pair<Object, Double>>();
      Double.max(2, 2);
      Double double2 = new Double(2.0);
      Pair<Object, Double> pair1 = new Pair<Object, Double>(iSAACRandom0, double2);
      linkedList0.add(pair1);
      Double.isFinite((-2333.542525));
      linkedList0.add(pair0);
      DiscreteDistribution<Object> discreteDistribution0 = new DiscreteDistribution<Object>(iSAACRandom0, linkedList0);
      Double double3 = new Double(2);
      discreteDistribution0.sample();
      Double.max((-1.0), 2);
      LinkedList<Pair<Object, Double>> linkedList1 = new LinkedList<Pair<Object, Double>>();
      linkedList0.add(pair0);
      DiscreteDistribution<Object> discreteDistribution1 = new DiscreteDistribution<Object>(iSAACRandom0, linkedList0);
      int int0 = (-709);
      // Undeclared exception!
      discreteDistribution1.sample(2);
  }
}
