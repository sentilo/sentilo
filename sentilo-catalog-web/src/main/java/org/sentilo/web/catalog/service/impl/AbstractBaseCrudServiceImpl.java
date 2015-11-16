/*
 * Sentilo
 *  
 * Original version 1.4 Copyright (C) 2013 Institut Municipal d’Informàtica, Ajuntament de Barcelona.
 * Modified by Opentrends adding support for multitenant deployments and SaaS. Modifications on version 1.5 Copyright (C) 2015 Opentrends Solucions i Sistemes, S.L.
 * 
 *   
 * This program is licensed and may be used, modified and redistributed under the
 * terms  of the European Public License (EUPL), either version 1.1 or (at your 
 * option) any later version as soon as they are approved by the European 
 * Commission.
 *   
 * Alternatively, you may redistribute and/or modify this program under the terms
 * of the GNU Lesser General Public License as published by the Free Software 
 * Foundation; either  version 3 of the License, or (at your option) any later 
 * version. 
 *   
 * Unless required by applicable law or agreed to in writing, software distributed
 * under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR 
 * CONDITIONS OF ANY KIND, either express or implied. 
 *   
 * See the licenses for the specific language governing permissions, limitations 
 * and more details.
 *   
 * You should have received a copy of the EUPL1.1 and the LGPLv3 licenses along 
 * with this program; if not, you may find them at: 
 *   
 *   https://joinup.ec.europa.eu/software/page/eupl/licence-eupl
 *   http://www.gnu.org/licenses/ 
 *   and 
 *   https://www.gnu.org/licenses/lgpl.txt
 */
package org.sentilo.web.catalog.service.impl;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import javax.annotation.PostConstruct;

import org.sentilo.web.catalog.domain.CatalogDocument;
import org.sentilo.web.catalog.exception.builder.DefaultCatalogBuilderExceptionImpl;
import org.sentilo.web.catalog.exception.builder.ResourceNotFoundExceptionBuilder;
import org.sentilo.web.catalog.search.SearchFilter;
import org.sentilo.web.catalog.search.SearchFilterResult;
import org.sentilo.web.catalog.security.audit.Auditable;
import org.sentilo.web.catalog.security.audit.AuditingActionType;
import org.sentilo.web.catalog.service.CrudService;
import org.sentilo.web.catalog.utils.TenantUtils;
import org.sentilo.web.catalog.validator.DefaultEntityKeyValidatorImpl;
import org.sentilo.web.catalog.validator.EntityKeyValidator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.dao.DataAccessException;
import org.springframework.data.mongodb.core.MongoOperations;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.data.mongodb.repository.MongoRepository;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

import com.mongodb.MongoException;

public abstract class AbstractBaseCrudServiceImpl<T extends CatalogDocument> extends AbstractBaseServiceImpl implements CrudService<T>, ApplicationContextAware {

  private static final Logger LOGGER = LoggerFactory.getLogger(AbstractBaseCrudServiceImpl.class);

  @Autowired
  private MongoOperations mongoOps;

  private ApplicationContext context;

  private final Class<T> type;
  private EntityKeyValidator entityKeyValidator;
  private ResourceNotFoundExceptionBuilder resourceNotFoundExceptionBuilder;

  public AbstractBaseCrudServiceImpl(final Class<T> type) {
    this.type = type;
  }

  @PostConstruct
  public void init() {
    this.setEntityKeyValidator(new DefaultEntityKeyValidatorImpl(type, getRepository()));
    this.setResourceNotFoundExceptionBuilder(new DefaultCatalogBuilderExceptionImpl(type));
    doAfterInit();
  }

  public abstract MongoRepository<T, String> getRepository();

  public abstract String getEntityId(T entity);

  /*
   * (non-Javadoc)
   * 
   * @see
   * org.springframework.context.ApplicationContextAware#setApplicationContext(org.springframework
   * .context.ApplicationContext)
   */
  @Override
  public void setApplicationContext(final ApplicationContext applicationContext) {
    context = applicationContext;
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.sentilo.web.catalog.service.CrudService#create(java.lang.Object)
   */
  @Override
  @Auditable(actionType = AuditingActionType.CREATE)
  public T create(final T entity) {
    doBeforeCreate(entity);
    checkIntegrityKey(entity.getId());
    return getRepository().save(entity);
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.sentilo.web.catalog.service.CrudService#update(java.lang.Object)
   */
  @Override
  @Auditable(actionType = AuditingActionType.UPDATE)
  public T update(final T entity) {
    return getRepository().save(entity);
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.sentilo.web.catalog.service.CrudService#insertAll(java.util.Collection)
   */
  @Override
  @Auditable(actionType = AuditingActionType.CREATE)
  public Collection<T> insertAll(final Collection<T> entities) throws MongoException, DataAccessException {
    // Este metodo permite hacer un insert de todos los elementos de la colección. Pero en caso de
    // que un elemento de esta ya este
    // registrado en el sistema lo que hace es interrumpir el alta sin lanzar ningún error.
    // Esto provoca que los elementos no leidos aun de la colección no se inserten.
    // Por ello hacemos una modificación y antes de invocar al metodo insert, filtramos los
    // elementos de la colección ya existentes: esto penaliza el rendimiento,
    // pero por otro lado garantiza la inserción de los elementos.
    final Collection<T> entitiesFiltered = removeFromCollectionIfAlreadyExists(entities);
    if (!entitiesFiltered.isEmpty()) {
      getMongoOps().insert(entitiesFiltered, this.type);
    }

    return entitiesFiltered;
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.sentilo.web.catalog.service.CrudService#updateAll(java.util.Collection)
   */
  @Override
  @Auditable(actionType = AuditingActionType.UPDATE)
  public Collection<T> updateAll(final Collection<T> entities) {
    // En el driver de Mongo no existe un update masivo de toda una coleccion, como si que existe el
    // insert masivo.
    // Por lo tanto, procedemos elemento a elemento. En caso de error en alguna actualizacion, se
    // marca el error y se prosigue con el update de los siguientes.
    final Collection<T> entitiesUpdated = new ArrayList<T>();
    if (!entities.isEmpty()) {
      for (final T entity : entities) {
        try {
          update(entity);
          entitiesUpdated.add(entity);
        } catch (final DataAccessException dae) {
          LOGGER.warn("Error updating entity with id {}", entity.getId());
        }
      }
    }

    return entitiesUpdated;
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.sentilo.web.catalog.service.CrudService#delete(java.lang.Object)
   */
  @Override
  @Auditable(actionType = AuditingActionType.DELETE)
  public void delete(final T entity) {
    getRepository().delete(entity);
    doAfterDelete(entity);
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.sentilo.web.catalog.service.CrudService#delete(java.util.Collection)
   */
  @Override
  @Auditable(actionType = AuditingActionType.DELETE)
  public void delete(final Collection<T> entities) {
    getRepository().delete(entities);
    doAfterDelete(entities);

  }

  /*
   * (non-Javadoc)
   * 
   * @see org.sentilo.web.catalog.service.CrudService#find(java.lang.Object)
   */
  @Override
  public T find(final T entity) {
    return getRepository().findOne(getEntityId(entity));
  }

  @Override
  public T findAndThrowErrorIfNotExist(final T entity) {
    final T entityToReturn = find(entity);
    if (entityToReturn == null) {
      getResourceNotFoundExceptionBuilder().buildResourceNotFoundException(entity.getId());
    }
    return entityToReturn;
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.sentilo.web.catalog.service.CrudService#findAll()
   */
  @Override
  public List<T> findAll() {
    if (applyFilterByTenant(this.type)) {
      final SearchFilter filter = new SearchFilter();
      filter.addAndParam("tenantId", TenantUtils.getCurrentTenant());
      return mongoOps.find(buildQuery(filter), this.type);
    } else {
      return getRepository().findAll();
    }
  }

  /*
   * (non-Javadoc)
   * 
   * @see
   * org.sentilo.web.catalog.service.CrudService#search(org.sentilo.web.catalog.utils.SearchFilter)
   */
  @Override
  public SearchFilterResult<T> search(final SearchFilter filter) {
    // El resultado debe ser un objeto Page<T>, el cual internamente contiene el listado a mostrar,
    // el total de registros y
    // el objeto Pageable original. En nuestro caso, como queremos el SearchFilter, lo que hacemos
    // es inspirarnos en esta
    // clase para crear la SearchFilterResult.
    final Query countQuery = buildCountQuery(filter);
    final Query query = buildQuery(filter);

    final long total = getMongoOps().count(countQuery, this.type);
    final List<T> content = getMongoOps().find(query, this.type);

    return new SearchFilterResult<T>(content, filter, total);
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.sentilo.web.catalog.service.CrudService#count()
   */
  @Override
  public Long count() {
    return getRepository().count();
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.sentilo.web.catalog.service.CrudService#exist(java.lang.String)
   */
  @Override
  public boolean exist(final String entityId) {
    return getRepository().exists(entityId);
  }

  protected void checkIntegrityKey(final String idToCheck) {
    getEntityKeyValidator().checkIntegrityKey(idToCheck);
  }

  protected void doBeforeCreate(final T entity) {
    // To override by subclasses.
  }

  protected void doAfterDelete(final T entity) {
    // To override by subclasses.
  }

  protected void doAfterDelete(final Collection<T> entities) {
    // To override by subclasses.
  }

  protected void doAfterInit() {
    // To override by subclasses
  }

  private Collection<T> removeFromCollectionIfAlreadyExists(final Collection<T> entities) {
    final Collection<T> entitiesFiltered = new ArrayList<T>();
    if (!CollectionUtils.isEmpty(entities)) {
      for (final T doc : entities) {
        // Si el doc no tiene id o bien no se encuentra, significa que no existe en el sistema
        if (!StringUtils.hasText(doc.getId()) || find(doc) == null) {
          entitiesFiltered.add(doc);
        }
      }
    }

    return entitiesFiltered;
  }

  public void setMongoOps(final MongoOperations mongoOps) {
    this.mongoOps = mongoOps;
  }

  public MongoOperations getMongoOps() {
    return mongoOps;
  }

  protected ApplicationContext getContext() {
    return context;
  }

  protected EntityKeyValidator getEntityKeyValidator() {
    return entityKeyValidator;
  }

  protected void setEntityKeyValidator(final EntityKeyValidator entityKeyValidator) {
    this.entityKeyValidator = entityKeyValidator;
  }

  protected ResourceNotFoundExceptionBuilder getResourceNotFoundExceptionBuilder() {
    return resourceNotFoundExceptionBuilder;
  }

  protected void setResourceNotFoundExceptionBuilder(final ResourceNotFoundExceptionBuilder resourceNotFoundExceptionBuilder) {
    this.resourceNotFoundExceptionBuilder = resourceNotFoundExceptionBuilder;
  }
}
