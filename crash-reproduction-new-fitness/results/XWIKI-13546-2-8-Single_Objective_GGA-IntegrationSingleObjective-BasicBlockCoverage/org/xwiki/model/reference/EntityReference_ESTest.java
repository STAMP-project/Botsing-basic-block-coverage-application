/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 13:40:27 UTC 2020
 */

package org.xwiki.model.reference;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.io.Serializable;
import java.util.Map;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;
import org.xwiki.model.EntityType;
import org.xwiki.model.reference.EntityReference;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class EntityReference_ESTest extends EntityReference_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      String string0 = "The old reference [";
      EntityType entityType0 = EntityType.OBJECT;
      EntityReference entityReference0 = new EntityReference("The old reference [", entityType0);
      Map<String, Serializable> map0 = entityReference0.getParameters();
      entityReference0.setParameters(map0);
      entityReference0.getReversedReferenceChain();
      entityReference0.hashCode();
      EntityType entityType1 = EntityType.BLOCK;
      EntityReference entityReference1 = new EntityReference("", entityType1);
  }
}
