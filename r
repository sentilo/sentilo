[1mdiff --git a/sentilo-catalog-web/src/main/java/org/sentilo/web/catalog/controller/CrudController.java b/sentilo-catalog-web/src/main/java/org/sentilo/web/catalog/controller/CrudController.java[m
[1mindex 08ac671..ae0e50c 100644[m
[1m--- a/sentilo-catalog-web/src/main/java/org/sentilo/web/catalog/controller/CrudController.java[m
[1m+++ b/sentilo-catalog-web/src/main/java/org/sentilo/web/catalog/controller/CrudController.java[m
[36m@@ -113,11 +113,7 @@[m [mpublic abstract class CrudController<T extends CatalogDocument> extends SearchCo[m
 	}[m
 [m
 	@RequestMapping(value = "/delete", method = RequestMethod.POST)[m
[31m-	public String deleteResource(@RequestParam String[] selectedIds, HttpServletRequest request, Model model) {[m
[31m-		//TODO Mikel: las llamadas a este metodo deberian ser vía AJAX, ya que se producen desde el listado y el retorno es al listado.[m
[31m-		// No tiene sentido la recarga de la página y además simplificaría las cosas.[m
[31m-		// El problema es que si es via AJAX, el contenido no se actualiza ... a no ser que se haga la llamada de nuevo via AJAX[m
[31m-		// o este sea el retorno en caso de exito del delete[m
[32m+[m	[32mpublic String deleteResource(@RequestParam String[] selectedIds, HttpServletRequest request, Model model) {[m[41m		[m
 		doBeforeDeleteResource(selectedIds, request, model);[m
 		getService().delete(buildResourceListFromIds(selectedIds));[m
 		addConfirmationMessageToModel(RESOURCE_DELETED, model);[m
[1mdiff --git a/sentilo-catalog-web/src/main/java/org/sentilo/web/catalog/service/impl/PermissionServiceImpl.java b/sentilo-catalog-web/src/main/java/org/sentilo/web/catalog/service/impl/PermissionServiceImpl.java[m
[1mindex 6ba6706..3263ed9 100644[m
[1m--- a/sentilo-catalog-web/src/main/java/org/sentilo/web/catalog/service/impl/PermissionServiceImpl.java[m
[1m+++ b/sentilo-catalog-web/src/main/java/org/sentilo/web/catalog/service/impl/PermissionServiceImpl.java[m
[36m@@ -129,9 +129,7 @@[m [mpublic class PermissionServiceImpl extends AbstractBaseServiceImpl<Permission> i[m
 	 * (non-Javadoc)[m
 	 * @see org.sentilo.web.catalog.service.PermissionService#getAuthorizedProviders(java.lang.String, java.lang.String)[m
 	 */[m
[31m-	public List<AuthorizedProvider> getAuthorizedProviders(String entityId, String sensorType){[m
[31m-		[m
[31m-		//TODO Mikel: Este metodo no parece encajar muy bien en este servicio. Replantearse localizacion[m
[32m+[m	[32mpublic List<AuthorizedProvider> getAuthorizedProviders(String entityId, String sensorType){[m[41m		[m
 		List<AuthorizedProvider> authorizedProviders = new ArrayList<AuthorizedProvider>();[m
 				[m
 		SearchFilter filter = new SearchFilter();[m
[1mdiff --git a/sentilo-platform/sentilo-platform-server/src/main/java/org/sentilo/platform/server/http/RequestListenerThread.java b/sentilo-platform/sentilo-platform-server/src/main/java/org/sentilo/platform/server/http/RequestListenerThread.java[m
[1mindex f09e91f..caa4062 100644[m
[1m--- a/sentilo-platform/sentilo-platform-server/src/main/java/org/sentilo/platform/server/http/RequestListenerThread.java[m
[1m+++ b/sentilo-platform/sentilo-platform-server/src/main/java/org/sentilo/platform/server/http/RequestListenerThread.java[m
[36m@@ -69,7 +69,7 @@[m [mpublic class RequestListenerThread extends Thread {[m
 [m
 	private final Logger logger = LoggerFactory.getLogger(RequestListenerThread.class);[m
 [m
[31m-	private ServerSocket socket;[m
[32m+[m	[32mprivate ServerSocket serverSocket;[m
 	private HttpParams params;[m
 	private HttpService httpService;[m
 [m
[36m@@ -109,12 +109,15 @@[m [mpublic class RequestListenerThread extends Thread {[m
 			while (notInterrupted()) {[m
 				manageConnection(new DefaultHttpServerConnection());[m
 			}[m
[31m-		} catch (IOException e) {[m
[31m-			logger.error("Error while initializing connection thread", e);[m
[31m-		} finally {[m
[32m+[m		[32m} catch (IOException ioe) {[m
[32m+[m			[32mlogger.error("Error while initializing connection thread. {}", ioe);[m
[32m+[m		[32m} catch (Throwable t){[m
[32m+[m			[32mlogger.error("Error while running sentilo server on port {}. {}", port, t);[m[41m			[m
[32m+[m		[32m}finally {[m
 			stopThreadPool();[m
[32m+[m			[32mreleaseSocketPort();[m
 		}[m
[31m-	}[m
[32m+[m	[32m}[m[41m	[m
 [m
 	private void initialize() throws IOException {[m
 		logger.info("Initializing server");[m
[36m@@ -151,7 +154,7 @@[m [mpublic class RequestListenerThread extends Thread {[m
 [m
 	private void initializeListener() throws IOException {[m
 		logger.info("Initializing listener on port {}",port);[m
[31m-		this.socket = new ServerSocket(port);[m
[32m+[m		[32mthis.serverSocket = new ServerSocket(port);[m
 	}[m
 [m
 	private void initializeThreadPool() {[m
[36m@@ -182,7 +185,7 @@[m [mpublic class RequestListenerThread extends Thread {[m
 	}[m
 [m
 	private void manageConnection(DefaultHttpServerConnection conn)	throws IOException {[m
[31m-		Socket s = this.socket.accept();[m
[32m+[m		[32mSocket s = this.serverSocket.accept();[m
 		conn.bind(s, params);[m
 		threadPool.submit(new SentiloThreadPoolExecutor(httpService, conn));[m
 	}[m
[36m@@ -192,9 +195,27 @@[m [mpublic class RequestListenerThread extends Thread {[m
 	}[m
 [m
 	private void stopThreadPool() {[m
[31m-		if (this.threadPool != null) {[m
[31m-			this.threadPool.shutdown();[m
[32m+[m		[32mtry{[m
[32m+[m			[32mif (this.threadPool != null) {[m
[32m+[m				[32mthis.threadPool.shutdown();[m
[32m+[m			[32m}[m
[32m+[m		[32m}catch(Throwable e){[m
[32m+[m			[32m//ignore the error[m
 		}[m
[32m+[m[41m		[m
[32m+[m		[32mlogger.info("Thread pool shutdown");[m[41m	[m
[32m+[m	[32m}[m
[32m+[m[41m	[m
[32m+[m	[32mprivate void releaseSocketPort() {[m
[32m+[m		[32mtry {[m
[32m+[m			[32mif (this.serverSocket.isClosed() == false) {[m
[32m+[m				[32mthis.serverSocket.close();[m
[32m+[m			[32m}[m
[32m+[m		[32m} catch (IOException e) {[m
[32m+[m			[32m//ignore the error[m
[32m+[m		[32m}[m[41m		[m
[32m+[m[41m		[m
[32m+[m		[32mlogger.info("Listener off on port {}", port);[m[41m				[m
 	}[m
 [m
 	public int getPort() {[m
