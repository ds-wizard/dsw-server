module Api.Resource.QuestionnaireMigrator.QuestionnaireMigratorStateCreateDTO where

import qualified Data.UUID as U

data QuestionnaireMigratorStateCreateDTO = QuestionnaireMigratorStateCreateDTO
  { _questionnaireMigratorStateCreateDTOTargetPackageId :: U.UUID
  }
