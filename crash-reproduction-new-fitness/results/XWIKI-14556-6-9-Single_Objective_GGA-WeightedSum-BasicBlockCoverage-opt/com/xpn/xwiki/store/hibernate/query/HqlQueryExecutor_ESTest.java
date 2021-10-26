/*
 * This file was automatically generated by EvoSuite
 * Tue Oct 26 06:05:13 UTC 2021
 */

package com.xpn.xwiki.store.hibernate.query;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import ch.qos.logback.classic.Logger;
import com.xpn.xwiki.store.hibernate.query.HqlQueryExecutor;
import com.xpn.xwiki.web.DeleteVersionsForm;
import com.xpn.xwiki.web.XWikiServletRequest;
import com.xpn.xwiki.web.XWikiServletResponseStub;
import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.apache.struts.chain.contexts.ServletActionContext;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.javaee.injection.Injector;
import org.hibernate.Query;
import org.hibernate.engine.SessionImplementor;
import org.hibernate.engine.query.OrdinalParameterDescriptor;
import org.hibernate.engine.query.ParameterMetadata;
import org.hibernate.impl.CollectionFilterImpl;
import org.junit.runner.RunWith;
import org.xwiki.component.embed.EmbeddableComponentManager;
import org.xwiki.query.SecureQuery;
import org.xwiki.query.internal.CountDocumentFilter;
import org.xwiki.query.internal.DefaultQuery;
import ucar.nc2.util.net.URLStreamHandlerFactory;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class HqlQueryExecutor_ESTest extends HqlQueryExecutor_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      HqlQueryExecutor hqlQueryExecutor0 = new HqlQueryExecutor();
      EmbeddableComponentManager embeddableComponentManager0 = new EmbeddableComponentManager();
      DefaultQuery defaultQuery0 = new DefaultQuery("2iVwl@Mznz", hqlQueryExecutor0);
      SecureQuery secureQuery0 = defaultQuery0.checkCurrentUser(false);
      String string0 = "queries.hbm.xml";
      CountDocumentFilter countDocumentFilter0 = new CountDocumentFilter();
      Logger logger0 = (Logger)URLStreamHandlerFactory.log;
      Injector.inject(countDocumentFilter0, (Class<?>) CountDocumentFilter.class, "logger", (Object) logger0);
      Injector.validateBean(countDocumentFilter0, (Class<?>) CountDocumentFilter.class);
      defaultQuery0.bindValue("queries.hbm.xml", (Object) countDocumentFilter0);
      OrdinalParameterDescriptor[] ordinalParameterDescriptorArray0 = new OrdinalParameterDescriptor[0];
      DeleteVersionsForm deleteVersionsForm0 = new DeleteVersionsForm();
      deleteVersionsForm0.getRequest();
      XWikiServletRequest xWikiServletRequest0 = new XWikiServletRequest((HttpServletRequest) null);
      xWikiServletRequest0.getServletContext();
      XWikiServletResponseStub xWikiServletResponseStub0 = new XWikiServletResponseStub();
      xWikiServletResponseStub0.getHttpServletResponse();
      ServletActionContext servletActionContext0 = new ServletActionContext((ServletContext) null, xWikiServletRequest0, (HttpServletResponse) null);
      ParameterMetadata parameterMetadata0 = new ParameterMetadata(ordinalParameterDescriptorArray0, servletActionContext0);
      CollectionFilterImpl collectionFilterImpl0 = new CollectionFilterImpl("|^q", (Object) null, (SessionImplementor) null, parameterMetadata0);
      Query query0 = collectionFilterImpl0.setCacheable(false);
      // Undeclared exception!
      hqlQueryExecutor0.populateParameters(query0, secureQuery0);
  }
}
