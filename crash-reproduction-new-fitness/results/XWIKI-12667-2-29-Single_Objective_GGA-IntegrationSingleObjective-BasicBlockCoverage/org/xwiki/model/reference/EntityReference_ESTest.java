/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 17:55:09 UTC 2020
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
      EntityType entityType0 = EntityType.SPACE;
      EntityType entityType1 = EntityType.BLOCK;
      EntityType entityType2 = EntityType.BLOCK;
      EntityReference entityReference0 = new EntityReference("BLOCK", entityType2);
      entityReference0.equalsNonRecursive(entityReference0);
      EntityReference entityReference1 = entityReference0.appendParent(entityReference0);
      entityReference0.getParameters();
      entityReference1.getName();
      entityReference1.hashCode();
      entityReference1.getType();
      EntityReference entityReference2 = new EntityReference("", entityType0, entityReference0);
  }
}
