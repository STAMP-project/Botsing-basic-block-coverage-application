/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 13:35:52 UTC 2020
 */

package org.xwiki.model.reference;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;
import org.xwiki.model.EntityType;
import org.xwiki.model.reference.EntityReference;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class EntityReference_ESTest extends EntityReference_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      EntityType entityType0 = EntityType.ATTACHMENT;
      EntityReference entityReference0 = new EntityReference("I\"+FWbi", entityType0);
      entityReference0.getParameters();
      String string0 = "";
      // Undeclared exception!
      entityReference0.setName("");
  }
}
