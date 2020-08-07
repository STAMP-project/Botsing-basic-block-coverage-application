/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 13:21:50 UTC 2020
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
      EntityType entityType0 = EntityType.CLASS_PROPERTY;
      EntityReference entityReference0 = new EntityReference("d8eb9|QypD:`UT8_R8p", entityType0);
      entityReference0.getParameter("");
      EntityReference entityReference1 = entityReference0.getRoot();
      entityReference1.setParameter("", "");
      EntityReference entityReference2 = new EntityReference(entityReference0);
      entityReference0.hashCode();
      entityReference2.getParameter("d8eb9|QypD:`UT8_R8p");
      entityReference1.getParent();
      Map<String, Serializable> map0 = entityReference2.getParameters();
      entityReference0.setParameters(map0);
      EntityType entityType1 = EntityType.SPACE;
      EntityReference entityReference3 = new EntityReference("", entityType1, entityReference1);
  }
}
