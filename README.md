# Podcast Royalty Sharing Contract
A Clarity smart contract for managing podcast revenue distribution among co-hosts.

## 🎯 Features

- Create podcasts with multiple hosts
- Manage revenue sharing percentages
- Distribute ad revenue and royalties
- Track earnings per host
- Withdraw accumulated earnings

## 📋 Contract Functions

### Core Functions

- `create-podcast`: Create a new podcast
- `add-host`: Add a co-host with their share percentage
- `update-share-percentage`: Modify a host's revenue share
- `distribute-revenue`: Distribute revenue among hosts
- `withdraw-earnings`: Withdraw accumulated earnings

### Read-Only Functions

- `get-podcast`: Get podcast details
- `get-host-info`: Get host information
- `get-distribution`: Get distribution details

## 🚀 Usage

1. Deploy the contract using Clarinet
2. Create a podcast using `create-podcast`
3. Add co-hosts with `add-host`
4. Distribute revenue using `distribute-revenue`
5. Hosts can withdraw earnings using `withdraw-earnings`

## ⚙️ Requirements

- Clarinet
- Stacks blockchain account

## 🔒 Security

- Only authorized hosts can add new co-hosts
- Share percentages must be valid (0-100)
- Contract owner controls share updates
- Sufficient funds required for distributions
```

