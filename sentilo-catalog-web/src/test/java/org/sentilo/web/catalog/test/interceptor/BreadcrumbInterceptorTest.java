package org.sentilo.web.catalog.test.interceptor;

import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.Spy;
import org.sentilo.common.test.AbstractBaseTest;
import org.sentilo.web.catalog.breadcrumb.BreadcrumbsTrail;
import org.sentilo.web.catalog.interceptor.BreadcrumbInterceptor;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.method.HandlerMethod;

public class BreadcrumbInterceptorTest extends AbstractBaseTest {

  @InjectMocks
  private BreadcrumbInterceptor interceptor;

  @Mock
  private HttpServletRequest request;

  @Mock
  private HttpServletResponse response;

  @Mock
  private HttpSession session;

  @Mock
  private HandlerMethod handler;

  @Mock
  private RequestMapping annotation;

  @Spy
  private BreadcrumbsTrail bcTrail = new BreadcrumbsTrail();

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);

    when(request.getSession()).thenReturn(session);
    when(session.getAttribute("currentBreadCrumb")).thenReturn(bcTrail);
  }

  @Test
  public void processNoAdminRequest() throws Exception {
    final String mockRefererUrl = "http://mock.sentilo.acme/mockList";
    when(request.getServletPath()).thenReturn("/mockResource");
    when(request.getHeader("referer")).thenReturn(mockRefererUrl);
    when(handler.getMethodAnnotation(RequestMapping.class)).thenReturn(null);
    interceptor.preHandle(request, response, handler);

    final BreadcrumbsTrail bcTrail = (BreadcrumbsTrail) session.getAttribute("currentBreadCrumb");

    Assert.assertTrue(bcTrail == null || !mockRefererUrl.equals(bcTrail.firstEntry()));

  }

  @Test
  public void processAdminRequest() throws Exception {
    final String mockRefererUrl = "http://mock.sentilo.acme/admin/mockList";
    when(request.getServletPath()).thenReturn("/admin/mockResource/detail");
    when(request.getHeader("referer")).thenReturn(mockRefererUrl);
    when(handler.getMethodAnnotation(RequestMapping.class)).thenReturn(annotation);
    when(annotation.method()).thenReturn(new RequestMethod[] {RequestMethod.GET});
    when(annotation.produces()).thenReturn(new String[] {});

    interceptor.preHandle(request, response, handler);

    final BreadcrumbsTrail bcTrail = (BreadcrumbsTrail) session.getAttribute("currentBreadCrumb");

    Assert.assertTrue(bcTrail != null && mockRefererUrl.equals(bcTrail.firstEntry()));

  }

  @Test
  public void deepNavigation() throws Exception {
    final String mockRefererUrl1 = "http://mock.sentilo.acme/admin/mockResourceList?sfamr=true&param1=value1";
    final String mockRefererUrl2 = "http://mock.sentilo.acme/admin/mockResource/detail";
    final String mockRefererUrl3 = "http://mock.sentilo.acme/admin/mockResource2List";
    final String mockRefererUrl4 = "http://mock.sentilo.acme/admin/mockResource2List/detail";

    final String mockRefererUrl1Filtered = "http://mock.sentilo.acme/admin/mockResourceList?param1=value1";

    when(request.getServletPath()).thenReturn("/admin/mock");
    when(request.getHeader("referer")).thenReturn(mockRefererUrl1, mockRefererUrl2, mockRefererUrl3, mockRefererUrl4);
    when(handler.getMethodAnnotation(RequestMapping.class)).thenReturn(annotation);
    when(annotation.method()).thenReturn(new RequestMethod[] {RequestMethod.GET});
    when(annotation.produces()).thenReturn(new String[] {});

    interceptor.preHandle(request, response, handler);
    interceptor.preHandle(request, response, handler);
    interceptor.preHandle(request, response, handler);
    interceptor.preHandle(request, response, handler);

    final BreadcrumbsTrail bcTrail = (BreadcrumbsTrail) session.getAttribute("currentBreadCrumb");

    verify(bcTrail, times(4)).push(anyString());

    Assert.assertTrue(bcTrail != null && mockRefererUrl4.equals(bcTrail.poll()));
    Assert.assertTrue(bcTrail != null && mockRefererUrl3.equals(bcTrail.poll()));
    Assert.assertTrue(bcTrail != null && mockRefererUrl2.equals(bcTrail.poll()));
    Assert.assertTrue(bcTrail != null && mockRefererUrl1Filtered.equals(bcTrail.poll()));
  }

  @Test
  public void fromAdminMenuRequest() throws Exception {
    when(request.getParameter("sfamr")).thenReturn("true");
    when(request.getServletPath()).thenReturn("/admin/mock");
    when(handler.getMethodAnnotation(RequestMapping.class)).thenReturn(annotation);
    when(annotation.method()).thenReturn(new RequestMethod[] {RequestMethod.GET});
    when(annotation.produces()).thenReturn(new String[] {});

    interceptor.preHandle(request, response, handler);

    final BreadcrumbsTrail bcTrail = (BreadcrumbsTrail) session.getAttribute("currentBreadCrumb");

    verify(bcTrail).clear();
    Assert.assertNull(bcTrail.firstEntry());

  }

  @Test
  public void fromBackRequest() throws Exception {
    final String mockRefererUrl = "http://mock.sentilo.acme/admin/mockList";
    when(request.getParameter("sfbr")).thenReturn("true");
    when(request.getServletPath()).thenReturn("/admin/mockResource/detail");
    when(request.getHeader("referer")).thenReturn(mockRefererUrl);
    when(handler.getMethodAnnotation(RequestMapping.class)).thenReturn(annotation);
    when(annotation.method()).thenReturn(new RequestMethod[] {RequestMethod.GET});
    when(annotation.produces()).thenReturn(new String[] {});

    interceptor.preHandle(request, response, handler);

    final BreadcrumbsTrail bcTrail = (BreadcrumbsTrail) session.getAttribute("currentBreadCrumb");

    verify(bcTrail).poll();
  }

}
