/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 00:54:25 UTC 2020
 */

package org.xwiki.extension.job.internal;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;
import org.xwiki.extension.job.internal.AbstractInstallPlanJob;
import org.xwiki.extension.job.plan.internal.DefaultExtensionPlanNode;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class AbstractInstallPlanJob$ModifableExtensionPlanTree_ESTest extends AbstractInstallPlanJob$ModifableExtensionPlanTree_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      AbstractInstallPlanJob.ModifableExtensionPlanTree abstractInstallPlanJob_ModifableExtensionPlanTree0 = new AbstractInstallPlanJob.ModifableExtensionPlanTree();
      AbstractInstallPlanJob.ModifableExtensionPlanTree abstractInstallPlanJob_ModifableExtensionPlanTree1 = new AbstractInstallPlanJob.ModifableExtensionPlanTree();
      DefaultExtensionPlanNode defaultExtensionPlanNode0 = new DefaultExtensionPlanNode();
      DefaultExtensionPlanNode defaultExtensionPlanNode1 = new DefaultExtensionPlanNode(defaultExtensionPlanNode0);
      abstractInstallPlanJob_ModifableExtensionPlanTree1.addIfAbsent(defaultExtensionPlanNode1);
      // Undeclared exception!
      abstractInstallPlanJob_ModifableExtensionPlanTree1.clone();
  }
}
