/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 15:54:44 UTC 2020
 */

package com.xpn.xwiki.store.hibernate.query;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import ch.qos.logback.classic.Logger;
import com.xpn.xwiki.store.hibernate.HibernateSessionFactory;
import com.xpn.xwiki.store.hibernate.query.HqlQueryExecutor;
import javax.inject.Provider;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.evosuite.runtime.javaee.injection.Injector;
import org.hibernate.Query;
import org.hibernate.engine.NamedQueryDefinition;
import org.hibernate.engine.SessionImplementor;
import org.hibernate.engine.query.OrdinalParameterDescriptor;
import org.hibernate.engine.query.ParameterMetadata;
import org.hibernate.impl.CollectionFilterImpl;
import org.hibernate.impl.SQLQueryImpl;
import org.hibernate.type.CurrencyType;
import org.infinispan.atomic.impl.AtomicHashMap;
import org.infinispan.context.Flag;
import org.junit.runner.RunWith;
import org.slf4j.event.SubstituteLoggingEvent;
import org.xwiki.component.embed.EmbeddableComponentManager;
import org.xwiki.component.internal.RootComponentManager;
import org.xwiki.configuration.ConfigurationSource;
import org.xwiki.context.Execution;
import org.xwiki.job.event.status.JobProgressManager;
import org.xwiki.query.internal.AbstractHiddenFilter;
import org.xwiki.query.internal.AbstractWhereQueryFilter;
import org.xwiki.query.internal.HiddenSpaceFilter;
import org.xwiki.rendering.block.IdBlock;
import org.xwiki.security.authorization.ContextualAuthorizationManager;
import ucar.nc2.util.net.URLStreamHandlerFactory;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class HqlQueryExecutor_ESTest extends HqlQueryExecutor_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      ContextualAuthorizationManager contextualAuthorizationManager0 = mock(ContextualAuthorizationManager.class, new ViolatedAssumptionAnswer());
      Provider<NamedQueryDefinition> provider0 = (Provider<NamedQueryDefinition>) mock(Provider.class, new ViolatedAssumptionAnswer());
      Execution execution0 = mock(Execution.class, new ViolatedAssumptionAnswer());
      JobProgressManager jobProgressManager0 = mock(JobProgressManager.class, new ViolatedAssumptionAnswer());
      HibernateSessionFactory hibernateSessionFactory0 = mock(HibernateSessionFactory.class, new ViolatedAssumptionAnswer());
      OrdinalParameterDescriptor[] ordinalParameterDescriptorArray0 = new OrdinalParameterDescriptor[1];
      CurrencyType currencyType0 = CurrencyType.INSTANCE;
      Flag[] flagArray0 = new Flag[8];
      Flag flag0 = Flag.SKIP_LOCKING;
      flagArray0[1] = flag0;
      SQLQueryImpl sQLQueryImpl0 = mock(SQLQueryImpl.class, new ViolatedAssumptionAnswer());
      HqlQueryExecutor hqlQueryExecutor0 = new HqlQueryExecutor();
      ContextualAuthorizationManager contextualAuthorizationManager1 = mock(ContextualAuthorizationManager.class, new ViolatedAssumptionAnswer());
      Injector.inject(hqlQueryExecutor0, (Class<?>) HqlQueryExecutor.class, "authorization", (Object) contextualAuthorizationManager1);
      Provider<IdBlock> provider1 = (Provider<IdBlock>) mock(Provider.class, new ViolatedAssumptionAnswer());
      Injector.inject(hqlQueryExecutor0, (Class<?>) HqlQueryExecutor.class, "componentManagerProvider", (Object) provider1);
      EmbeddableComponentManager embeddableComponentManager0 = new EmbeddableComponentManager("~i");
      SubstituteLoggingEvent substituteLoggingEvent0 = new SubstituteLoggingEvent();
      SessionImplementor sessionImplementor0 = mock(SessionImplementor.class, new ViolatedAssumptionAnswer());
      AtomicHashMap.ProxyMode atomicHashMap_ProxyMode0 = AtomicHashMap.ProxyMode.COARSE;
      AtomicHashMap<Object, RootComponentManager> atomicHashMap0 = new AtomicHashMap<Object, RootComponentManager>(atomicHashMap_ProxyMode0);
      ParameterMetadata parameterMetadata0 = new ParameterMetadata(ordinalParameterDescriptorArray0, atomicHashMap0);
      CollectionFilterImpl collectionFilterImpl0 = new CollectionFilterImpl("{0^f,gJ8 ", substituteLoggingEvent0, sessionImplementor0, parameterMetadata0);
      Query query0 = collectionFilterImpl0.setTimeout(117);
      HiddenSpaceFilter hiddenSpaceFilter0 = new HiddenSpaceFilter();
      ConfigurationSource configurationSource0 = mock(ConfigurationSource.class, new ViolatedAssumptionAnswer());
      Injector.inject(hiddenSpaceFilter0, (Class<?>) AbstractHiddenFilter.class, "userPreferencesSource", (Object) configurationSource0);
      Logger logger0 = (Logger)URLStreamHandlerFactory.log;
      Injector.inject(hiddenSpaceFilter0, (Class<?>) AbstractWhereQueryFilter.class, "logger", (Object) logger0);
      Injector.validateBean(hiddenSpaceFilter0, (Class<?>) HiddenSpaceFilter.class);
      // Undeclared exception!
      hqlQueryExecutor0.setNamedParameter(query0, "\"Pk\"f}c'>", hiddenSpaceFilter0);
  }
}
