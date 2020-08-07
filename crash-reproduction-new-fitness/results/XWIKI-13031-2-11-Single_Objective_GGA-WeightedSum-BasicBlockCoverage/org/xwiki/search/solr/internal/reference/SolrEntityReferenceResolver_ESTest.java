/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 14:21:28 UTC 2020
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
import org.xwiki.model.internal.reference.ExplicitReferenceEntityReferenceResolver;
import org.xwiki.model.reference.EntityReferenceResolver;
import org.xwiki.search.solr.internal.reference.SolrEntityReferenceResolver;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class SolrEntityReferenceResolver_ESTest extends SolrEntityReferenceResolver_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      EntityReferenceResolver<DefaultReferenceEntityReferenceResolver> entityReferenceResolver0 = (EntityReferenceResolver<DefaultReferenceEntityReferenceResolver>) mock(EntityReferenceResolver.class, new ViolatedAssumptionAnswer());
      EntityType entityType0 = EntityType.ATTACHMENT;
      SolrEntityReferenceResolver solrEntityReferenceResolver0 = new SolrEntityReferenceResolver();
      EntityReferenceResolver<ExplicitReferenceEntityReferenceResolver> entityReferenceResolver1 = (EntityReferenceResolver<ExplicitReferenceEntityReferenceResolver>) mock(EntityReferenceResolver.class, new ViolatedAssumptionAnswer());
      EntityType entityType1 = EntityType.SPACE;
      SolrEntityReferenceResolver solrEntityReferenceResolver1 = new SolrEntityReferenceResolver();
      EntityReferenceResolver<DefaultReferenceEntityReferenceResolver> entityReferenceResolver2 = (EntityReferenceResolver<DefaultReferenceEntityReferenceResolver>) mock(EntityReferenceResolver.class, new ViolatedAssumptionAnswer());
      Injector.inject(solrEntityReferenceResolver1, (Class<?>) SolrEntityReferenceResolver.class, "explicitReferenceEntityReferenceResolver", (Object) entityReferenceResolver2);
      Injector.validateBean(solrEntityReferenceResolver1, (Class<?>) SolrEntityReferenceResolver.class);
      SolrDocument solrDocument0 = new SolrDocument();
      EntityType entityType2 = EntityType.SPACE;
      solrDocument0.put("wiki", (Object) entityReferenceResolver1);
      Object[] objectArray0 = new Object[1];
      Object object0 = new Object();
      objectArray0[0] = object0;
      // Undeclared exception!
      solrEntityReferenceResolver1.resolve(solrDocument0, entityType2, objectArray0);
  }
}
