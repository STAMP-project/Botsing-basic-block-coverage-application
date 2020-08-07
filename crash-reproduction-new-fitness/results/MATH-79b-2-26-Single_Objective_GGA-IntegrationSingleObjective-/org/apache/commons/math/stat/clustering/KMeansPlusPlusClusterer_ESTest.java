/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 17:06:13 UTC 2020
 */

package org.apache.commons.math.stat.clustering;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.util.LinkedList;
import java.util.Random;
import org.apache.commons.math.stat.clustering.EuclideanIntegerPoint;
import org.apache.commons.math.stat.clustering.KMeansPlusPlusClusterer;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class KMeansPlusPlusClusterer_ESTest extends KMeansPlusPlusClusterer_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      LinkedList<EuclideanIntegerPoint> linkedList0 = new LinkedList<EuclideanIntegerPoint>();
      LinkedList<EuclideanIntegerPoint> linkedList1 = new LinkedList<EuclideanIntegerPoint>();
      Random random0 = new Random();
      random0.nextInt();
      KMeansPlusPlusClusterer<EuclideanIntegerPoint> kMeansPlusPlusClusterer0 = new KMeansPlusPlusClusterer<EuclideanIntegerPoint>(random0);
      int[] intArray0 = new int[4];
      intArray0[0] = (-1001);
      intArray0[1] = 1072513955;
      intArray0[2] = (-1001);
      intArray0[3] = 1072513955;
      EuclideanIntegerPoint euclideanIntegerPoint0 = new EuclideanIntegerPoint(intArray0);
      linkedList1.add(euclideanIntegerPoint0);
      EuclideanIntegerPoint euclideanIntegerPoint1 = new EuclideanIntegerPoint(intArray0);
      linkedList1.add(euclideanIntegerPoint1);
      kMeansPlusPlusClusterer0.cluster(linkedList1, (-1001), 7);
      linkedList1.add(euclideanIntegerPoint1);
      kMeansPlusPlusClusterer0.cluster(linkedList1, (-1001), 19);
      // Undeclared exception!
      kMeansPlusPlusClusterer0.cluster(linkedList0, 13, 1072513955);
  }
}
