/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 12:51:28 UTC 2020
 */

package org.xwiki.model.internal.reference;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.util.Map;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.javaee.injection.Injector;
import org.junit.runner.RunWith;
import org.xwiki.model.EntityType;
import org.xwiki.model.internal.reference.AbstractStringEntityReferenceResolver;
import org.xwiki.model.internal.reference.DefaultSymbolScheme;
import org.xwiki.model.internal.reference.ExplicitStringEntityReferenceResolver;
import org.xwiki.model.internal.reference.SymbolScheme;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class AbstractStringEntityReferenceResolver_ESTest extends AbstractStringEntityReferenceResolver_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      ExplicitStringEntityReferenceResolver explicitStringEntityReferenceResolver0 = new ExplicitStringEntityReferenceResolver();
      DefaultSymbolScheme defaultSymbolScheme0 = new DefaultSymbolScheme();
      defaultSymbolScheme0.initialize();
      Injector.inject(explicitStringEntityReferenceResolver0, (Class<?>) AbstractStringEntityReferenceResolver.class, "symbolScheme", (Object) defaultSymbolScheme0);
      Injector.validateBean(explicitStringEntityReferenceResolver0, (Class<?>) AbstractStringEntityReferenceResolver.class);
      explicitStringEntityReferenceResolver0.initialize();
      SymbolScheme symbolScheme0 = explicitStringEntityReferenceResolver0.getSymbolScheme();
      EntityType entityType0 = EntityType.ATTACHMENT;
      EntityType entityType1 = EntityType.WIKI;
      EntityType entityType2 = EntityType.OBJECT;
      Map<Character, EntityType> map0 = explicitStringEntityReferenceResolver0.getTypeSetup(entityType2);
      explicitStringEntityReferenceResolver0.initialize();
      explicitStringEntityReferenceResolver0.getTypeSetup(entityType2);
      EntityType entityType3 = EntityType.ATTACHMENT;
      Object[] objectArray0 = new Object[7];
      objectArray0[0] = (Object) entityType0;
      objectArray0[1] = (Object) symbolScheme0;
      objectArray0[2] = (Object) entityType1;
      objectArray0[3] = (Object) entityType1;
      Object object0 = new Object();
      objectArray0[4] = object0;
      objectArray0[5] = (Object) defaultSymbolScheme0;
      objectArray0[6] = (Object) map0;
      // Undeclared exception!
      explicitStringEntityReferenceResolver0.resolve("Y1", entityType3, objectArray0);
  }
}
