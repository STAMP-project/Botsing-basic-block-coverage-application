/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 12:55:33 UTC 2020
 */

package org.xwiki.model.internal.reference;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;
import org.xwiki.model.EntityType;
import org.xwiki.model.internal.reference.ExplicitStringEntityReferenceResolver;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class AbstractEntityReferenceResolver_ESTest extends AbstractEntityReferenceResolver_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      ExplicitStringEntityReferenceResolver explicitStringEntityReferenceResolver0 = new ExplicitStringEntityReferenceResolver();
      EntityType entityType0 = EntityType.CLASS_PROPERTY;
      Object[] objectArray0 = new Object[8];
      objectArray0[0] = (Object) entityType0;
      objectArray0[1] = (Object) explicitStringEntityReferenceResolver0;
      objectArray0[2] = (Object) entityType0;
      objectArray0[3] = (Object) entityType0;
      objectArray0[4] = (Object) entityType0;
      objectArray0[5] = (Object) entityType0;
      objectArray0[6] = (Object) explicitStringEntityReferenceResolver0;
      objectArray0[7] = (Object) explicitStringEntityReferenceResolver0;
      // Undeclared exception!
      explicitStringEntityReferenceResolver0.resolveDefaultReference(entityType0, objectArray0);
  }
}
