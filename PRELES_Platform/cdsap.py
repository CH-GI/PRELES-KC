import os
import csv
import netCDF4 as nc


def nc_to_csv(nc_file_path, csv_file_path):
    # 打开NC文件
    dataset = nc.Dataset(nc_file_path, 'r')

    # 获取所有变量的名称
    variables = list(dataset.variables.keys())

    # 获取数据的维度信息
    dimensions = dataset.dimensions

    # 找到最大的维度，假设数据沿着这个维度展开
    max_dim = None
    max_dim_size = 0
    for dim_name, dim in dimensions.items():
        if dim.size > max_dim_size:
            max_dim = dim_name
            max_dim_size = dim.size

    if max_dim is None:
        raise ValueError("No dimensions found in the NC file.")

    # 打开CSV文件并写入表头
    with open(csv_file_path, 'w', newline='') as csvfile:
        writer = csv.writer(csvfile)

        # 写入表头
        writer.writerow(variables)

        # 逐行读取数据并写入CSV
        for i in range(max_dim_size):
            row = []
            for var_name in variables:
                var = dataset.variables[var_name]
                # 检查变量是否包含最大维度
                if max_dim in var.dimensions:
                    # 如果变量包含最大维度，逐行读取数据
                    if len(var.shape) == 1:
                        # 确保索引在范围内
                        if i < var.shape[0]:
                            row.append(var[i])
                        else:
                            row.append(None)  # 超出范围时用 None 填充
                    else:
                        # 处理多维变量，找到最大维度的索引位置
                        dim_index = var.dimensions.index(max_dim)
                        if i < var.shape[dim_index]:
                            # 使用 numpy 的索引方式提取数据
                            index = [slice(None)] * len(var.shape)
                            index[dim_index] = i
                            value = var[tuple(index)]
                            row.append(value.item() if value.size == 1 else value.tolist())
                        else:
                            row.append(None)  # 超出范围时用 None 填充
                else:
                    # 如果变量不包含最大维度，直接取值
                    row.append(var[:].item() if var[:].size == 1 else var[:].tolist())
            writer.writerow(row)

    print(f"Converted {nc_file_path} to {csv_file_path}")


def process_nc_files_in_current_path():
    # 获取当前脚本所在路径
    folder_path = os.path.dirname(os.path.abspath(__file__))

    # 遍历指定路径下的所有文件
    for file_name in os.listdir(folder_path):
        if file_name.endswith(".nc"):  # 检查是否为NC文件
            nc_file_path = os.path.join(folder_path, file_name)
            csv_file_path = os.path.join(folder_path, file_name.replace(".nc", ".csv"))
            nc_to_csv(nc_file_path, csv_file_path)
            print(f"Converted {file_name} to {csv_file_path}")


# 自动处理当前路径下的所有NC文件
process_nc_files_in_current_path()