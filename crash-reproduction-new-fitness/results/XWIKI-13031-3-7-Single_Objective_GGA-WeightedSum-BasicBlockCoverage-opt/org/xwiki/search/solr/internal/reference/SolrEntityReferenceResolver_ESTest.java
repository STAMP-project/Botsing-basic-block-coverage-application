/*
 * This file was automatically generated by EvoSuite
 * Mon Oct 25 16:22:19 UTC 2021
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
import org.xwiki.model.internal.reference.DefaultStringEntityReferenceResolver;
import org.xwiki.model.reference.EntityReferenceResolver;
import org.xwiki.search.solr.internal.reference.SolrEntityReferenceResolver;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class SolrEntityReferenceResolver_ESTest extends SolrEntityReferenceResolver_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      EntityReferenceResolver<DefaultStringEntityReferenceResolver> entityReferenceResolver0 = (EntityReferenceResolver<DefaultStringEntityReferenceResolver>) mock(EntityReferenceResolver.class, new ViolatedAssumptionAnswer());
      EntityType entityType0 = EntityType.ATTACHMENT;
      EntityType entityType1 = EntityType.SPACE;
      Object[] objectArray0 = null;
      SolrDocument solrDocument0 = new SolrDocument();
      Object[] objectArray1 = new Object[0];
      solrDocument0.put("wiki", (Object) entityType0);
      SolrEntityReferenceResolver solrEntityReferenceResolver0 = new SolrEntityReferenceResolver();
      EntityReferenceResolver<DefaultReferenceEntityReferenceResolver> entityReferenceResolver1 = (EntityReferenceResolver<DefaultReferenceEntityReferenceResolver>) mock(EntityReferenceResolver.class, new ViolatedAssumptionAnswer());
      Injector.inject(solrEntityReferenceResolver0, (Class<?>) SolrEntityReferenceResolver.class, "explicitReferenceEntityReferenceResolver", (Object) entityReferenceResolver1);
      Injector.validateBean(solrEntityReferenceResolver0, (Class<?>) SolrEntityReferenceResolver.class);
      EntityType entityType2 = EntityType.CLASS_PROPERTY;
      Object[] objectArray2 = new Object[4];
      objectArray2[0] = (Object) entityType2;
      objectArray2[1] = (Object) entityType1;
      objectArray2[2] = (Object) entityReferenceResolver0;
      objectArray2[3] = (Object) entityType1;
      // Undeclared exception!
      solrEntityReferenceResolver0.resolve(solrDocument0, entityType2, objectArray2);
  }
}
