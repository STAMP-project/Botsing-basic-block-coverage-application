/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 18:47:49 UTC 2020
 */

package org.xwiki.search.solr.internal.reference;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.solr.common.SolrDocument;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.evosuite.runtime.javaee.injection.Injector;
import org.junit.runner.RunWith;
import org.xwiki.model.EntityType;
import org.xwiki.model.internal.reference.DefaultReferenceEntityReferenceResolver;
import org.xwiki.model.internal.reference.ExplicitStringEntityReferenceResolver;
import org.xwiki.model.internal.reference.RelativeStringEntityReferenceResolver;
import org.xwiki.model.reference.EntityReferenceResolver;
import org.xwiki.search.solr.internal.reference.SolrEntityReferenceResolver;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class SolrEntityReferenceResolver_ESTest extends SolrEntityReferenceResolver_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      DefaultReferenceEntityReferenceResolver defaultReferenceEntityReferenceResolver0 = new DefaultReferenceEntityReferenceResolver();
      EntityType entityType0 = EntityType.ATTACHMENT;
      Object object0 = new Object();
      ExplicitStringEntityReferenceResolver explicitStringEntityReferenceResolver0 = new ExplicitStringEntityReferenceResolver();
      Object object1 = new Object();
      SolrDocument solrDocument0 = new SolrDocument();
      solrDocument0.put("wiki", (Object) entityType0);
      SolrEntityReferenceResolver solrEntityReferenceResolver0 = new SolrEntityReferenceResolver();
      EntityReferenceResolver<RelativeStringEntityReferenceResolver> entityReferenceResolver0 = (EntityReferenceResolver<RelativeStringEntityReferenceResolver>) mock(EntityReferenceResolver.class, new ViolatedAssumptionAnswer());
      Injector.inject(solrEntityReferenceResolver0, (Class<?>) SolrEntityReferenceResolver.class, "explicitReferenceEntityReferenceResolver", (Object) entityReferenceResolver0);
      Injector.validateBean(solrEntityReferenceResolver0, (Class<?>) SolrEntityReferenceResolver.class);
      EntityType entityType1 = EntityType.ATTACHMENT;
      Object[] objectArray0 = new Object[7];
      objectArray0[0] = (Object) "uA*<&xZr{f4Kn@";
      objectArray0[1] = (Object) solrDocument0;
      objectArray0[2] = (Object) solrDocument0;
      objectArray0[3] = object1;
      objectArray0[4] = (Object) explicitStringEntityReferenceResolver0;
      objectArray0[5] = (Object) null;
      objectArray0[6] = (Object) entityType1;
      // Undeclared exception!
      solrEntityReferenceResolver0.resolve(solrDocument0, entityType1, objectArray0);
  }
}
