/*
 * Sentilo
 *   
 * Copyright (C) 2013 Institut Municipal d’Informàtica, Ajuntament de  Barcelona.
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
import java.util.Set;

import javax.annotation.PostConstruct;

import org.sentilo.web.catalog.domain.CatalogDocument;
import org.sentilo.web.catalog.exception.builder.DefaultCatalogBuilderExceptionImpl;
import org.sentilo.web.catalog.exception.builder.ResourceNotFoundExceptionBuilder;
import org.sentilo.web.catalog.search.SearchFilter;
import org.sentilo.web.catalog.search.SearchFilterResult;
import org.sentilo.web.catalog.service.CrudService;
import org.sentilo.web.catalog.validator.DefaultEntityKeyValidatorImpl;
import org.sentilo.web.catalog.validator.EntityKeyValidator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataAccessException;
import org.springframework.data.mongodb.core.MongoOperations;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.data.mongodb.repository.MongoRepository;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

import com.mongodb.MongoException;


public abstract class AbstractBaseServiceImpl<T extends CatalogDocument> implements CrudService<T>{
	
	private final Logger logger = LoggerFactory.getLogger(AbstractBaseServiceImpl.class);
	
	@Autowired
	private MongoOperations mongoOps;
	
	private final Class<T> type;	
	protected EntityKeyValidator entityKeyValidator;
	protected ResourceNotFoundExceptionBuilder resourceNotFoundExceptionBuilder;
	
	public AbstractBaseServiceImpl(Class<T> type){
		this.type = type;				
	}
	
	@PostConstruct
	public final void init(){
		this.entityKeyValidator = new DefaultEntityKeyValidatorImpl(type, getRepository());
		this.resourceNotFoundExceptionBuilder = new DefaultCatalogBuilderExceptionImpl(type);
		doAfterInit();		
	}
	
	protected void doAfterInit(){
		//To override by subclasses
	}
	
	public abstract MongoRepository<T, String> getRepository();
	public abstract String getEntityId(T entity);			
	
	/*
	 * (non-Javadoc)
	 * @see org.sentilo.web.catalog.service.CrudService#create(java.lang.Object)
	 */
	public T create(T entity) {			
		checkIntegrityKey(entity.getId());
		return getRepository().save(entity);
	}
	
	/*
	 * (non-Javadoc)
	 * @see org.sentilo.web.catalog.service.CrudService#update(java.lang.Object)
	 */
	public T update(T entity) {		
		return getRepository().save(entity);
	}
	
	/*
	 * (non-Javadoc)
	 * @see org.sentilo.web.catalog.service.CrudService#insertAll(java.util.Collection)
	 */
	public void insertAll(Collection<T> entities) throws MongoException, DataAccessException{		
		// Este metodo permite hacer un insert de todos los elementos de la colección. Pero en caso de que un elemento de esta ya este
		// registrado en el sistema lo que hace es interrumpir el alta sin lanzar ningún error.
		// Esto provoca que los elementos no leidos aun de la colección no se inserten.
		// Por ello hacemos una modificación y antes de invocar al metodo insert, filtramos los elementos de la colección ya existentes: esto penaliza el rendimiento, 
		// pero por otro lado garantiza la inserción de los elementos.
		Collection<T> entitiesFiltered = removeFromCollectionIfAlreadyExists(entities);	
		if(!entitiesFiltered.isEmpty()){
			getMongoOps().insert(entitiesFiltered, this.type);
		}
	}
	
	
	/*
	 * (non-Javadoc)
	 * @see org.sentilo.web.catalog.service.CrudService#updateAll(java.util.Collection)
	 */
	public void updateAll(Collection<T> entities) {
		// En el driver de Mongo no existe un update masivo de toda una coleccion, como si que existe el insert masivo.
		// Por lo tanto, procedemos elemento a elemento. En caso de error en alguna actualizacion, se marca el error y se prosigue con 
		// el update de los siguientes. 		
				
		if(!entities.isEmpty()){
			for(T entity : entities){
				try{					
					update(entity);
				}catch(DataAccessException dae){
					logger.warn("Error updating entity with id {}", entity.getId());
				}
			}
		}
	}
		

	/*
	 * (non-Javadoc)
	 * @see org.sentilo.web.catalog.service.CrudService#delete(java.lang.Object)
	 */
	public void delete(T entity) {
		getRepository().delete(entity);		
	}
		
	/*
	 * (non-Javadoc)
	 * @see org.sentilo.web.catalog.service.CrudService#delete(java.util.Collection)
	 */
	public void delete(Collection<T> entities) {
		getRepository().delete(entities);
		
	}
	
	/*
	 * (non-Javadoc)
	 * @see org.sentilo.web.catalog.service.CrudService#find(java.lang.Object)
	 */
	public T find(T entity) {
		return getRepository().findOne(getEntityId(entity));
	}
	
	public T findAndThrowErrorIfNotExist(T entity){
		T entityToReturn = find(entity);
		if(entityToReturn == null){
			resourceNotFoundExceptionBuilder.buildResourceNotFoundException(entity.getId());
		}
		return entityToReturn;
	}
	
	/*
	 * (non-Javadoc)
	 * @see org.sentilo.web.catalog.service.CrudService#findAll()
	 */
	public List<T> findAll(){
		return getRepository().findAll();
	}

	/*
	 * (non-Javadoc)
	 * @see org.sentilo.web.catalog.service.CrudService#search(org.sentilo.web.catalog.utils.SearchFilter)
	 */
	public SearchFilterResult<T> search(SearchFilter filter) {
		// El resultado debe ser un objeto Page<T>, el cual internamente contiene el listado a mostrar, el total de registros y
		// el objeto Pageable original. En nuestro caso, como queremos el SearchFilter, lo que hacemos es inspirarnos en esta
		// clase para crear la SearchFilterResult.
		Query countQuery = buildCountQuery(filter);
		Query query = buildQuery(filter);
		
		long total = getMongoOps().count(countQuery, this.type);
		List<T> content = getMongoOps().find(query, this.type);
		
		return new SearchFilterResult<T>(content, filter, total);		
	}
		
	/*
	 * (non-Javadoc)
	 * @see org.sentilo.web.catalog.service.CrudService#count()
	 */
	public Long count(){
		return getRepository().count();
	}
	
	protected void checkIntegrityKey(String idToCheck) {		
		entityKeyValidator.checkIntegrityKey(idToCheck);
	}		
		
	protected Query buildQuery(SearchFilter filter){		
		return buildQuery(filter, true);		
	}
	
	protected Query buildCountQuery(SearchFilter filter){		
		return buildQuery(filter, false);
	}
	
	protected Query buildQuery(SearchFilter filter, boolean pageable){
				
		Criteria queryCriteria = new Criteria();
		
		if(!filter.andParamsIsEmpty()){			
			// andParams contiene la lista de filtros a aplicar en modo conjuncion, es decir, con el operador AND
			// Además, la comparativa del valor siempre es mediante "es exactamente este valor", es decir, se debe comportar como un EQUALS. 
			Set<String> andParams = filter.getAndParams().keySet();		
			Criteria[] aCriteria = new Criteria[andParams.size()]; 
			int i=0;
			for(String param: andParams){				
				aCriteria[i] = Criteria.where(param).is(filter.getAndParams().get(param));
				i++;
											
			}
			
			queryCriteria = queryCriteria.andOperator(aCriteria);
		}
		
		
		if(!filter.paramsIsEmpty()){
			// params contiene la lista de filtros a aplicar en modo disjuncion, es decir, con el operador OR
			// Además, la comparativa del valor siempre es mediante "contiene la palabra buscada", es decir, se debe comportar como un LIKE %value% en SQL 
			Set<String> params = filter.getParams().keySet();		
			Criteria[] aCriteria = new Criteria[params.size()]; 
			int i=0;
			for(String param: params){
				String regexp = ".*"+filter.getParams().get(param)+".*";			
				aCriteria[i] = Criteria.where(param).regex(regexp);
				i++;
											
			}
			
			queryCriteria = queryCriteria.orOperator(aCriteria);
		}
		
		Query query = new Query(queryCriteria);
		
		if(pageable){
			query.with(filter.getPageable());
		}
		
		return query;
	}
	
	protected Query buildQueryForIdInCollection(Collection<String> values){				
		return buildQueryForParamInCollection("id", values);
	}
	
	protected Query buildQueryForParamInCollection(String paramName, Collection<String> values){
		Criteria queryCriteria = Criteria.where(paramName).in(values);		
		return  new Query(queryCriteria);
	}
	
	private Collection<T> removeFromCollectionIfAlreadyExists(Collection<T> entities){
		Collection<T> entitiesFiltered = new ArrayList<T>();
		if(!CollectionUtils.isEmpty(entities)){
			for(T doc: entities){				
				// Si el doc no tiene id o bien no se encuentra, significa que no existe en el sistema
				if(!StringUtils.hasText(doc.getId()) || find(doc)==null){
					entitiesFiltered.add(doc);
				}				
			}
		}
		
		return entitiesFiltered;
	}		
		
	public void setMongoOps(MongoOperations mongoOps) {
		this.mongoOps = mongoOps;
	}

	public MongoOperations getMongoOps() {
		return mongoOps;
	}	 	

}
