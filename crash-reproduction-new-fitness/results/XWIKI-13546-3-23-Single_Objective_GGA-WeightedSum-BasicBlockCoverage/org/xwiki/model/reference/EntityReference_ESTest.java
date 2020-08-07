/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 18:02:17 UTC 2020
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
      EntityType entityType0 = EntityType.OBJECT_PROPERTY;
      EntityReference entityReference0 = new EntityReference("y#n]C<bs(Ht>Agg", entityType0);
      String string0 = "' u7oU<j[`fAQ9";
      EntityType entityType1 = EntityType.CLASS_PROPERTY;
      EntityType entityType2 = EntityType.SPACE;
      EntityReference entityReference1 = new EntityReference("", entityType1);
  }
}
