/*
 * This file was automatically generated by EvoSuite
 * Tue Mar 31 15:00:45 UTC 2020
 */

package org.xwiki.extension.job.internal;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.junit.runner.RunWith;
import org.xwiki.extension.ExtensionId;
import org.xwiki.extension.job.internal.AbstractInstallPlanJob;
import org.xwiki.extension.job.internal.UpgradePlanJob;
import org.xwiki.extension.job.plan.ExtensionPlanNode;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class UpgradePlanJob_ESTest extends UpgradePlanJob_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      UpgradePlanJob upgradePlanJob0 = new UpgradePlanJob();
      AbstractInstallPlanJob.ModifableExtensionPlanTree abstractInstallPlanJob_ModifableExtensionPlanTree0 = new AbstractInstallPlanJob.ModifableExtensionPlanTree();
      upgradePlanJob0.extensionTree = abstractInstallPlanJob_ModifableExtensionPlanTree0;
      ExtensionPlanNode extensionPlanNode0 = mock(ExtensionPlanNode.class, new ViolatedAssumptionAnswer());
      abstractInstallPlanJob_ModifableExtensionPlanTree0.add(extensionPlanNode0);
      ExtensionId extensionId0 = new ExtensionId("$wE,Cg", "");
      // Undeclared exception!
      upgradePlanJob0.tryInstallExtension(extensionId0, "Wildcard should have only one upper bound.");
  }
}
