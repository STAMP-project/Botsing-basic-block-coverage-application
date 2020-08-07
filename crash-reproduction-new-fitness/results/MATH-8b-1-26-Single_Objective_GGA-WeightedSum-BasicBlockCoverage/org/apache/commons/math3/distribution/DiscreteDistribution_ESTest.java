/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 02:51:52 UTC 2020
 */

package org.apache.commons.math3.distribution;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.util.LinkedList;
import org.apache.commons.math3.distribution.DiscreteDistribution;
import org.apache.commons.math3.random.MersenneTwister;
import org.apache.commons.math3.random.Well44497a;
import org.apache.commons.math3.util.Pair;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class DiscreteDistribution_ESTest extends DiscreteDistribution_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      LinkedList<Pair<Integer, Double>> linkedList0 = new LinkedList<Pair<Integer, Double>>();
      Integer integer0 = new Integer((-17));
      Double double0 = new Double(1.275277853012085);
      Pair<Integer, Double> pair0 = new Pair<Integer, Double>(integer0, double0);
      Pair<Integer, Double> pair1 = new Pair<Integer, Double>(integer0, double0);
      linkedList0.add(pair1);
      int[] intArray0 = new int[6];
      Well44497a well44497a0 = new Well44497a(intArray0);
      Short short0 = new Short((short)301);
      Pair<Short, Double> pair2 = new Pair<Short, Double>(short0, double0);
      Pair<Object, Double> pair3 = new Pair<Object, Double>(pair2, double0);
      LinkedList<Pair<Object, Double>> linkedList1 = new LinkedList<Pair<Object, Double>>();
      linkedList1.add(pair3);
      DiscreteDistribution<Object> discreteDistribution0 = new DiscreteDistribution<Object>(well44497a0, linkedList1);
      Short short1 = new Short((short)301);
      Pair<Short, Double> pair4 = new Pair<Short, Double>(short1, double0);
      Double double1 = new Double((short)301);
      Pair<Object, Double> pair5 = new Pair<Object, Double>(well44497a0, double1);
      LinkedList<Pair<Object, Double>> linkedList2 = new LinkedList<Pair<Object, Double>>();
      linkedList1.add(pair5);
      DiscreteDistribution<Object> discreteDistribution1 = new DiscreteDistribution<Object>(well44497a0, linkedList1);
      LinkedList<Pair<Byte, Double>> linkedList3 = new LinkedList<Pair<Byte, Double>>();
      MersenneTwister mersenneTwister0 = new MersenneTwister(0L);
      LinkedList<Pair<Integer, Double>> linkedList4 = new LinkedList<Pair<Integer, Double>>();
      DiscreteDistribution<Integer> discreteDistribution2 = new DiscreteDistribution<Integer>(well44497a0, linkedList0);
      linkedList4.add(pair1);
      discreteDistribution2.sample(1);
      // Undeclared exception!
      discreteDistribution1.sample((int) (short)301);
  }
}
