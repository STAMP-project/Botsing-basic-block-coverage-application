/**
 * Scaffolding file used to store all the setups needed to run 
 * tests automatically generated by EvoSuite
 * Mon Mar 30 16:48:23 UTC 2020
 */

package com.xpn.xwiki.store.hibernate.query;

import org.evosuite.runtime.annotation.EvoSuiteClassExclude;
import org.junit.BeforeClass;
import org.junit.Before;
import org.junit.After;

import static org.evosuite.shaded.org.mockito.Mockito.*;
@EvoSuiteClassExclude
public class HqlQueryExecutor_ESTest_scaffolding {

  @org.junit.Rule 
  public org.evosuite.runtime.vnet.NonFunctionalRequirementRule nfr = new org.evosuite.runtime.vnet.NonFunctionalRequirementRule();

  private org.evosuite.runtime.thread.ThreadStopper threadStopper =  new org.evosuite.runtime.thread.ThreadStopper (org.evosuite.runtime.thread.KillSwitchHandler.getInstance(), 3000);


  @BeforeClass 
  public static void initEvoSuiteFramework() { 
    org.evosuite.runtime.RuntimeSettings.className = "com.xpn.xwiki.store.hibernate.query.HqlQueryExecutor"; 
    org.evosuite.runtime.GuiSupport.initialize(); 
    org.evosuite.runtime.RuntimeSettings.maxNumberOfIterationsPerLoop = 10000; 
    org.evosuite.runtime.RuntimeSettings.mockSystemIn = true; 
    org.evosuite.runtime.Runtime.getInstance().resetRuntime(); 
    try { initMocksToAvoidTimeoutsInTheTests(); } catch(ClassNotFoundException e) {} 
  } 

  @Before 
  public void initTestCase(){ 
    threadStopper.storeCurrentThreads();
    threadStopper.startRecordingTime();
    org.evosuite.runtime.GuiSupport.setHeadless(); 
    org.evosuite.runtime.Runtime.getInstance().resetRuntime(); 
    org.evosuite.runtime.agent.InstrumentingAgent.activate(); 
  } 

  @After 
  public void doneWithTestCase(){ 
    threadStopper.killAndJoinClientThreads();
    org.evosuite.runtime.agent.InstrumentingAgent.deactivate(); 
    org.evosuite.runtime.GuiSupport.restoreHeadlessMode(); 
  } 


  private static void initializeClasses() {
    org.evosuite.runtime.classhandling.ClassStateSupport.initializeClasses(HqlQueryExecutor_ESTest_scaffolding.class.getClassLoader() ,
      "com.xpn.xwiki.XWikiException",
      "org.apache.http.conn.scheme.LayeredSocketFactory",
      "org.xwiki.security.authorization.Right",
      "org.xwiki.component.phase.Initializable",
      "org.xwiki.query.QueryFilter",
      "org.xwiki.query.Query",
      "org.apache.http.impl.conn.HttpConnPool",
      "org.xwiki.model.reference.EntityReference",
      "org.xwiki.component.phase.InitializationException",
      "com.xpn.xwiki.store.hibernate.query.HqlQueryExecutor",
      "org.apache.http.impl.conn.SystemDefaultDnsResolver",
      "org.xwiki.component.manager.CompatibilityComponentManager",
      "org.hibernate.Query",
      "org.apache.http.HttpMessage",
      "org.apache.http.conn.ssl.SSLSocketFactory",
      "org.apache.http.pool.AbstractConnPool",
      "org.apache.http.conn.HttpInetSocketAddress",
      "org.apache.http.conn.ClientConnectionManager",
      "org.apache.http.conn.scheme.SchemeSocketFactory",
      "com.xpn.xwiki.store.XWikiHibernateStore",
      "org.apache.http.impl.conn.SchemeRegistryFactory",
      "org.apache.http.HttpInetConnection",
      "org.apache.http.HttpRequest",
      "org.apache.http.pool.ConnPoolControl",
      "org.apache.http.pool.AbstractConnPool$1",
      "org.apache.http.conn.ssl.AllowAllHostnameVerifier",
      "org.xwiki.security.authorization.AccessDeniedException",
      "org.apache.http.conn.socket.ConnectionSocketFactory",
      "org.apache.http.impl.conn.HttpPoolEntry",
      "org.apache.http.client.CredentialsProvider",
      "org.apache.http.params.SyncBasicHttpParams",
      "ucar.nc2.util.net.URLStreamHandlerFactory",
      "org.apache.http.conn.scheme.PlainSocketFactory",
      "org.apache.http.pool.RouteSpecificPool",
      "org.apache.http.pool.PoolEntry",
      "org.apache.http.impl.conn.HttpConnPool$InternalConnFactory",
      "org.apache.http.params.AbstractHttpParams",
      "org.hibernate.Session",
      "org.xwiki.component.manager.ComponentLookupException",
      "org.xwiki.security.authorization.RightDescription",
      "org.apache.http.client.methods.HttpUriRequest",
      "org.xwiki.security.authorization.ContextualAuthorizationManager",
      "org.apache.http.conn.ssl.AbstractVerifier",
      "org.xwiki.query.internal.DefaultQuery",
      "org.apache.http.conn.ConnectTimeoutException",
      "org.apache.http.client.HttpClient",
      "org.apache.http.impl.conn.PoolingClientConnectionManager",
      "com.xpn.xwiki.store.XWikiHibernateBaseStore$HibernateCallback",
      "org.xwiki.context.Execution",
      "org.xwiki.query.QueryExecutor",
      "org.apache.http.HttpClientConnection",
      "org.apache.http.conn.ConnectionPoolTimeoutException",
      "org.apache.http.conn.ssl.SSLContexts",
      "org.apache.http.pool.ConnPool",
      "org.apache.http.protocol.HttpContext",
      "com.xpn.xwiki.XWikiContext",
      "org.apache.http.params.HttpParams",
      "org.apache.http.impl.client.DefaultHttpClient",
      "org.apache.http.params.BasicHttpParams",
      "ucar.httpservices.HTTPSession$Settings",
      "org.apache.http.conn.scheme.LayeredSchemeSocketFactory",
      "com.xpn.xwiki.store.XWikiStoreInterface",
      "org.apache.http.conn.scheme.SchemeLayeredSocketFactory",
      "org.apache.http.conn.ssl.StrictHostnameVerifier",
      "org.xwiki.component.annotation.Role",
      "org.apache.http.conn.ssl.X509HostnameVerifier",
      "org.xwiki.query.SecureQuery",
      "org.apache.http.conn.ssl.SSLInitializationException",
      "org.apache.http.params.HttpParamsNames",
      "ucar.httpservices.HTTPSession",
      "org.apache.http.util.Args",
      "org.apache.http.HttpEntity",
      "org.apache.http.pool.PoolEntryCallback",
      "org.xwiki.security.authorization.AuthorizationException",
      "org.apache.http.conn.socket.LayeredConnectionSocketFactory",
      "org.apache.http.conn.OperatedClientConnection",
      "org.apache.http.pool.ConnFactory",
      "org.apache.http.conn.scheme.Scheme",
      "org.apache.http.conn.ssl.BrowserCompatHostnameVerifier",
      "org.xwiki.component.manager.ComponentManager",
      "com.xpn.xwiki.store.hibernate.HibernateSessionFactory",
      "org.apache.http.conn.scheme.SocketFactory",
      "ucar.httpservices.HTTPException",
      "org.xwiki.query.internal.ScriptQuery",
      "org.xwiki.query.QueryException",
      "org.xwiki.query.internal.CountDocumentFilter",
      "org.xwiki.component.annotation.Component",
      "org.apache.http.conn.DnsResolver",
      "org.apache.http.conn.ClientConnectionRequest",
      "org.xwiki.query.internal.AbstractQueryFilter",
      "org.apache.http.conn.ClientConnectionOperator",
      "org.apache.http.impl.client.AbstractHttpClient",
      "ucar.httpservices.CustomSSLProtocolSocketFactory",
      "org.apache.http.conn.scheme.SchemeRegistry",
      "org.apache.http.impl.client.CloseableHttpClient",
      "org.apache.http.conn.ManagedClientConnection",
      "org.apache.http.conn.HttpRoutedConnection",
      "com.xpn.xwiki.store.XWikiHibernateBaseStore",
      "org.apache.http.impl.conn.DefaultClientConnectionOperator",
      "org.xwiki.job.event.status.JobProgressManager",
      "org.apache.http.conn.ConnectionReleaseTrigger",
      "org.apache.http.HttpConnection",
      "org.apache.http.conn.ManagedHttpClientConnection"
    );
  } 
  private static void initMocksToAvoidTimeoutsInTheTests() throws ClassNotFoundException { 
    mock(Class.forName("org.xwiki.security.authorization.ContextualAuthorizationManager", false, HqlQueryExecutor_ESTest_scaffolding.class.getClassLoader()));
  }
}
