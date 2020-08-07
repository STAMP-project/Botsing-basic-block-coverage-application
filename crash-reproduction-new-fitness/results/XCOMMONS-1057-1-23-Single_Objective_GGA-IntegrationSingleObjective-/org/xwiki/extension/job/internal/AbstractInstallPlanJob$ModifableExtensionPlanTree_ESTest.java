/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 05:19:57 UTC 2020
 */

package org.xwiki.extension.job.internal;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.util.Collection;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;
import org.xwiki.extension.ExtensionId;
import org.xwiki.extension.InstalledExtension;
import org.xwiki.extension.job.internal.AbstractInstallPlanJob;
import org.xwiki.extension.job.plan.ExtensionPlanAction;
import org.xwiki.extension.job.plan.internal.DefaultExtensionPlanAction;
import org.xwiki.extension.job.plan.internal.DefaultExtensionPlanNode;
import org.xwiki.extension.test.EmptyExtension;
import org.xwiki.extension.version.internal.DefaultVersion;
import org.xwiki.extension.version.internal.DefaultVersionConstraint;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class AbstractInstallPlanJob$ModifableExtensionPlanTree_ESTest extends AbstractInstallPlanJob$ModifableExtensionPlanTree_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      AbstractInstallPlanJob.ModifableExtensionPlanTree abstractInstallPlanJob_ModifableExtensionPlanTree0 = new AbstractInstallPlanJob.ModifableExtensionPlanTree();
      DefaultVersion defaultVersion0 = new DefaultVersion((String) null);
      DefaultVersion defaultVersion1 = new DefaultVersion(defaultVersion0);
      ExtensionId extensionId0 = new ExtensionId((String) null, defaultVersion1);
      EmptyExtension emptyExtension0 = new EmptyExtension(extensionId0, (String) null);
      ExtensionPlanAction.Action extensionPlanAction_Action0 = ExtensionPlanAction.Action.REPAIR;
      DefaultExtensionPlanAction defaultExtensionPlanAction0 = new DefaultExtensionPlanAction(emptyExtension0, (Collection<InstalledExtension>) null, extensionPlanAction_Action0, "type parameter", false);
      DefaultVersionConstraint defaultVersionConstraint0 = new DefaultVersionConstraint(defaultVersion0);
      DefaultVersionConstraint defaultVersionConstraint1 = new DefaultVersionConstraint(defaultVersionConstraint0);
      DefaultExtensionPlanNode defaultExtensionPlanNode0 = new DefaultExtensionPlanNode(defaultExtensionPlanAction0, defaultVersionConstraint1);
      abstractInstallPlanJob_ModifableExtensionPlanTree0.addIfAbsent(defaultExtensionPlanNode0);
      // Undeclared exception!
      abstractInstallPlanJob_ModifableExtensionPlanTree0.clone();
  }
}
